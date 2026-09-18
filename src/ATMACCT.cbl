      *****************************************************************
      * PROGRAM : ATMACCT
      * PURPOSE : 口座アクセスモジュール (勘定系ホストへの接続)
      * DESIGN  :
      *   - 口座へのアクセスはこのモジュールに一元化する。上位は口座が
      *     どこにあるかを知らない。元帳を端末側のファイルに置いていた
      *     構成から、ホスト接続 (ATMHOST) へ移したが、上位から見た
      *     インタフェース (ACCTIF.cpy) は変えていない。ATMPOST の検証
      *     順序も無変更である。
      *   - このモジュールは業務判定を持たない。口座状態 (解約・凍結・
      *     休眠) の判定は ATMPOST の検証順序の中で行う。READ の副作用
      *     として業務エラーを返すと、検証順序を ATMPOST に固定すると
      *     いう設計が成立しなくなるため。
      *
      *   [READLOCK はロックを取らない]
      *   ホストにレコードロックは無い。照会時の版数と現物をホストが
      *   比べることで排他する。READLOCK と READ の違いは、更新の前提
      *   として版数を控えるかどうかだけになった。上位が READLOCK →
      *   UPDATE の順で呼ぶ規律は、そのまま版数の受け渡しとして働く。
      *
      *   [成否不明はここで解決する]
      *   ホストの応答が返らない場合、記帳されたかどうかは端末には
      *   判らない。失敗として扱うと、記帳済みの取引を取り消さずに
      *   閉じることになる。取引照会 (QUERY) で結果を確かめ、記帳されて
      *   いなければ失敗、記帳されていれば成功として上位へ返す。
      *   照会でも決着しなければ RC-FATAL とし、上位が EJ に残して
      *   係員対応へ回す。不明を不明のまま上へ流さない。
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMACCT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *    -- 照会で受け取った版数。更新要求に載せてホストへ返す。
       01  WS-SAVED-VERSION            PIC 9(09) VALUE ZERO.

      *    -- 呼出元が返してきたレコードから版数を取り出すための第 2 像。
      *    -- レイアウトはコピー句から得るので、項目追加でずれない。
       COPY 'ACCTREC.cpy' REPLACING LEADING ==ACCT-== BY ==CHK-==.

       COPY 'RETCODE.cpy'.
       COPY 'HOSTIF.cpy'.

       LINKAGE SECTION.
       COPY 'ACCTIF.cpy'.
       COPY 'ATMSESS.cpy'.

       PROCEDURE DIVISION USING ACCT-PARM ATM-SESSION.

       MAIN-CONTROL SECTION.
       MAIN-START.
           MOVE RC-OK   TO ACCT-OUT-RETCODE
           MOVE EC-NONE TO ACCT-OUT-ERROR-CODE

           EVALUATE TRUE
               WHEN ACCT-FN-READ    PERFORM READ-ACCT
               WHEN ACCT-FN-LOCK    PERFORM READ-ACCT-LOCK
               WHEN ACCT-FN-UPDATE  PERFORM UPDATE-ACCT
               WHEN ACCT-FN-UNLOCK  CONTINUE
               WHEN ACCT-FN-CLOSE   PERFORM CLOSE-ACCT
               WHEN OTHER
                   MOVE RC-FATAL TO ACCT-OUT-RETCODE
           END-EVALUATE
           GOBACK.

       READ-ACCT SECTION.
       READ-A-START.
           SET HOST-FN-INQUIRY TO TRUE
           PERFORM CALL-HOST
           IF HOST-OUT-RETCODE = RC-OK
               PERFORM PUBLISH-RECORD
           END-IF.
       READ-A-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * READLOCK : 更新を前提とした照会。ロックは取らず、版数を控える。
      *----------------------------------------------------------------
       READ-ACCT-LOCK SECTION.
       READ-AL-START.
           PERFORM READ-ACCT.
       READ-AL-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * UPDATE : 記帳要求。成否不明なら取引照会で決着させる。
      *----------------------------------------------------------------
       UPDATE-ACCT SECTION.
       UPDATE-A-START.
      *    -- 呼出元が照会したときの版数を載せて送る。ホストが現物と
      *    -- 比べて排他する。
           MOVE ACCT-IO-RECORD TO CHK-RECORD
           IF CHK-VERSION NOT = WS-SAVED-VERSION
               MOVE RC-BUSINESS-ERROR TO ACCT-OUT-RETCODE
               MOVE EC-SYSTEM-BUSY    TO ACCT-OUT-ERROR-CODE
               GO TO UPDATE-A-EXIT
           END-IF

           SET HOST-FN-POST TO TRUE
           PERFORM CALL-HOST

           IF HOST-OC-UNKNOWN
               PERFORM RESOLVE-UNKNOWN
           END-IF

           IF HOST-OUT-RETCODE = RC-OK
               PERFORM PUBLISH-RECORD
           END-IF.
       UPDATE-A-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 成否不明の解決。記帳されたかどうかをホストに問い直す。
      *   見つかれば記帳済み。ただし現在値は応答に含まれないので、
      *   照会し直して呼出元へ返す値を揃える。
      *   見つからなければ記帳されていないと断定してよい。
      *   問い直しそのものが失敗したら RC-FATAL。不明のまま上へ流すと、
      *   上位は失敗と区別できず、記帳済みの取引を取り消さずに閉じる。
      *----------------------------------------------------------------
       RESOLVE-UNKNOWN SECTION.
       RSU-START.
           SET HOST-FN-QUERY TO TRUE
           PERFORM CALL-HOST

           IF HOST-OUT-RETCODE = RC-IO-ERROR
               MOVE RC-FATAL          TO ACCT-OUT-RETCODE
               MOVE EC-HOST-NO-ANSWER TO ACCT-OUT-ERROR-CODE
               GO TO RSU-EXIT
           END-IF

           IF HOST-FOUND-YES
      *        -- 記帳済み。現在値を取り直して成功として返す。
               SET HOST-FN-INQUIRY TO TRUE
               PERFORM CALL-HOST
           ELSE
      *        -- 記帳されていない。取引は成立しなかった。
               MOVE RC-BUSINESS-ERROR TO ACCT-OUT-RETCODE
               MOVE EC-HOST-NO-ANSWER TO ACCT-OUT-ERROR-CODE
           END-IF.
       RSU-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 電文の組み立てと送信。取引 ID は冪等キーとしてホストが使う。
      * 再送でも同じ値を送る (採番し直すと二重記帳になる)。
      *----------------------------------------------------------------
       CALL-HOST SECTION.
       CH-START.
           MOVE ACCT-IN-ACCT-NO TO HOST-IN-ACCT-NO
           MOVE SESS-TXN-ID     TO HOST-IN-TXN-ID
           MOVE WS-SAVED-VERSION TO HOST-IN-VERSION
           MOVE ACCT-IO-RECORD  TO HOST-IO-RECORD

           CALL 'ATMHOST' USING HOST-PARM ATM-SESSION

           MOVE HOST-OUT-RETCODE    TO ACCT-OUT-RETCODE
           MOVE HOST-OUT-ERROR-CODE TO ACCT-OUT-ERROR-CODE.
       CH-EXIT.
           EXIT.

       CLOSE-ACCT SECTION.
       CLOSE-A-START.
           SET HOST-FN-CLOSE TO TRUE
           CALL 'ATMHOST' USING HOST-PARM ATM-SESSION.
       CLOSE-A-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 共通: ホストが返したレコードを呼出元へ渡し、版数を控える
      *----------------------------------------------------------------
       PUBLISH-RECORD SECTION.
       PUB-START.
           MOVE HOST-IO-RECORD TO ACCT-IO-RECORD
           MOVE HOST-IO-RECORD TO CHK-RECORD
           MOVE CHK-VERSION    TO WS-SAVED-VERSION.
       PUB-EXIT.
           EXIT.

       END PROGRAM ATMACCT.
