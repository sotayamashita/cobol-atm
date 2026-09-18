      *****************************************************************
      * PROGRAM : ATMHOST
      * PURPOSE : 勘定系ホスト接続の模擬 (口座元帳の所有者)
      * DESIGN  :
      *   全銀システムを ATMZGN が模擬するのと同じ形で、勘定系ホストを
      *   模擬する。口座元帳ファイルを所有し、端末からは電文でしか
      *   触れない。端末側に元帳を置いていた構成との違いは、応答が
      *   返らない場合があることと、排他がホスト側にあることの 2 点。
      *
      *   [冪等性]
      *   同じ HOST-IN-TXN-ID の POST は、二度目以降も一度目と同じ結果
      *   を返す。端末は応答が返らなければ再送するしかないので、再送を
      *   安全にするのはホストの責務である。処理済みの取引 ID と結果を
      *   取引ログに残し、再送はそれを引いて答える。
      *
      *   [排他]
      *   端末が照会時に受け取った版数と現物を比べる。不一致なら他端末が
      *   先に更新しているので 9002 (混雑) を返す。ローカル元帳のときに
      *   ATMACCT が持っていた楽観ロックが、そのままここへ移る。
      *   レコードロックは持たない。ホストは 1 つなので、照会と記帳の
      *   間に他端末が割り込んでも版数で検出できる。
      *
      *   [成否不明の模擬]
      *   特定の口座への記帳で応答を返さない。ATMZGN が特定の金額で
      *   タイムアウトを模擬するのと同じ仕掛けで、端末側の復旧経路を
      *   試験できるようにするためのもの。
      *
      *   [取引ログの寿命]
      *   本実装の取引ログはメモリ上で、プロセスの間だけ生きる。実機の
      *   ホストは元帳と同じ永続性で持つ。ここで求めているのは「応答が
      *   返らなかった端末の再送を安全にする」ことで、その再送は同一
      *   セッション内に起きるため、模擬としてはこれで足りる。
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMHOST.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-FILE ASSIGN TO 'data/atmacct.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-NO
               FILE STATUS IS WS-ACCT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-FILE.
       COPY 'ACCTREC.cpy'.

       WORKING-STORAGE SECTION.
       01  WS-ACCT-STATUS              PIC X(02) VALUE '00'.
       01  WS-OPENED                   PIC X(01) VALUE 'N'.

      *    -- 応答を返さない口座。端末側の復旧経路を試験するための
      *    -- 仕掛けで、業務上の意味は無い。
       01  WS-TIMEOUT-ACCT             PIC X(10) VALUE '1000000009'.

      *    -- 取引ログ。処理済みの取引 ID と、そのときの結果。
       01  WS-LOG-TABLE.
           05  WS-LOG-MAX              PIC S9(04) COMP VALUE 500.
           05  WS-LOG-CNT              PIC S9(04) COMP VALUE ZERO.
           05  WS-LOG-ENTRY OCCURS 500 TIMES.
               10  WS-LOG-TXN-ID       PIC X(12).
               10  WS-LOG-OUTCOME      PIC X(01).
               10  WS-LOG-ERROR-CODE   PIC X(04).

       01  WS-WORK.
           05  WS-I                    PIC S9(04) COMP VALUE ZERO.
           05  WS-FOUND                PIC S9(04) COMP VALUE ZERO.

      *    -- 端末が返してきたレコードから版数を取り出すための第 2 像。
      *    -- レイアウトはコピー句から得るので、項目追加でずれない。
       COPY 'ACCTREC.cpy' REPLACING LEADING ==ACCT-== BY ==CHK-==.

       COPY 'RETCODE.cpy'.

       LINKAGE SECTION.
       COPY 'HOSTIF.cpy'.
       COPY 'ATMSESS.cpy'.

       PROCEDURE DIVISION USING HOST-PARM ATM-SESSION.

       MAIN-CONTROL SECTION.
       MAIN-START.
           MOVE RC-OK   TO HOST-OUT-RETCODE
           MOVE EC-NONE TO HOST-OUT-ERROR-CODE
           SET  HOST-OC-SUCCESS TO TRUE
           MOVE 'N'     TO HOST-OUT-FOUND

           EVALUATE TRUE
               WHEN HOST-FN-INQUIRY PERFORM DO-INQUIRY
               WHEN HOST-FN-POST    PERFORM DO-POST
               WHEN HOST-FN-QUERY   PERFORM DO-QUERY
               WHEN HOST-FN-CLOSE   PERFORM CLOSE-LEDGER
               WHEN OTHER
                   MOVE RC-FATAL TO HOST-OUT-RETCODE
                   SET  HOST-OC-FAILURE TO TRUE
           END-EVALUATE
           GOBACK.

       OPEN-LEDGER SECTION.
       OPEN-L-START.
           IF WS-OPENED = 'Y'
               GO TO OPEN-L-EXIT
           END-IF
           OPEN I-O ACCT-FILE
           IF WS-ACCT-STATUS = '00'
               MOVE 'Y' TO WS-OPENED
           ELSE
               MOVE RC-IO-ERROR  TO HOST-OUT-RETCODE
               MOVE EC-SYSTEM-IO TO HOST-OUT-ERROR-CODE
               SET  HOST-OC-FAILURE TO TRUE
           END-IF.
       OPEN-L-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * INQUIRY : 口座照会。業務判定はしない。
      *   口座状態 (解約・凍結・休眠) の判定は端末側の検証順序の中で
      *   行う。ホストが READ の副作用として業務エラーを返すと、検証
      *   順序を ATMPOST に固定するという設計が成立しなくなる。
      *----------------------------------------------------------------
       DO-INQUIRY SECTION.
       INQ-START.
           PERFORM OPEN-LEDGER
           IF HOST-OUT-RETCODE NOT = RC-OK
               GO TO INQ-EXIT
           END-IF

           MOVE HOST-IN-ACCT-NO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   MOVE RC-NOTFOUND     TO HOST-OUT-RETCODE
                   MOVE EC-ACCT-UNKNOWN TO HOST-OUT-ERROR-CODE
                   SET  HOST-OC-FAILURE TO TRUE
               NOT INVALID KEY
                   MOVE ACCT-RECORD TO HOST-IO-RECORD
           END-READ.
       INQ-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * POST : 記帳。版数を照合してから確定させる。
      *   再送 (同一取引 ID) は一度目と同じ結果を返す。二重記帳しない。
      *----------------------------------------------------------------
       DO-POST SECTION.
       POST-START.
      *    -- 再送かどうかを先に見る。元帳へ触る前に答えを決める。
           PERFORM FIND-LOG
           IF WS-FOUND > ZERO
               PERFORM REPLAY-LOG
               GO TO POST-EXIT
           END-IF

           PERFORM OPEN-LEDGER
           IF HOST-OUT-RETCODE NOT = RC-OK
               GO TO POST-EXIT
           END-IF

      *    -- 応答を返さない口座。記帳するかどうかは端末には判らない。
      *    -- 実機のタイムアウトと同じく、ここでは記帳せずに黙る。
           IF HOST-IN-ACCT-NO = WS-TIMEOUT-ACCT
               MOVE RC-BUSINESS-ERROR TO HOST-OUT-RETCODE
               MOVE EC-HOST-NO-ANSWER TO HOST-OUT-ERROR-CODE
               SET  HOST-OC-UNKNOWN   TO TRUE
               GO TO POST-EXIT
           END-IF

           MOVE HOST-IN-ACCT-NO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   MOVE RC-NOTFOUND     TO HOST-OUT-RETCODE
                   MOVE EC-ACCT-UNKNOWN TO HOST-OUT-ERROR-CODE
                   SET  HOST-OC-FAILURE TO TRUE
                   GO TO POST-EXIT
           END-READ

      *    -- 照会時の版数と現物が違えば、他端末が先に更新している。
           IF ACCT-VERSION NOT = HOST-IN-VERSION
               MOVE RC-BUSINESS-ERROR TO HOST-OUT-RETCODE
               MOVE EC-SYSTEM-BUSY    TO HOST-OUT-ERROR-CODE
               SET  HOST-OC-FAILURE   TO TRUE
               GO TO POST-EXIT
           END-IF

           MOVE HOST-IO-RECORD TO ACCT-RECORD
           ADD 1 TO ACCT-VERSION
           MOVE SESS-BUSINESS-DATE TO ACCT-LAST-TXN-DATE

           REWRITE ACCT-RECORD
               INVALID KEY
                   MOVE RC-IO-ERROR  TO HOST-OUT-RETCODE
                   MOVE EC-SYSTEM-IO TO HOST-OUT-ERROR-CODE
                   SET  HOST-OC-FAILURE TO TRUE
               NOT INVALID KEY
                   MOVE ACCT-RECORD TO HOST-IO-RECORD
           END-REWRITE

           PERFORM ADD-LOG.
       POST-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * QUERY : 取引結果の照会。成否不明を解決する唯一の手段。
      *   原取引が見つからなければ記帳されていない。端末はそれを根拠に
      *   取引を失敗として閉じられる。
      *----------------------------------------------------------------
       DO-QUERY SECTION.
       QRY-START.
           PERFORM FIND-LOG
           IF WS-FOUND > ZERO
               MOVE 'Y' TO HOST-OUT-FOUND
               PERFORM REPLAY-LOG
           ELSE
               MOVE 'N' TO HOST-OUT-FOUND
               SET  HOST-OC-FAILURE TO TRUE
           END-IF.
       QRY-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 取引ログ。記帳が成立したものだけを残す。失敗は残さない。
      *   失敗を残すと、再送のたびに同じ失敗を返すことになり、混雑
      *   などの一時的な失敗から回復できなくなる。
      *----------------------------------------------------------------
       ADD-LOG SECTION.
       ADDL-START.
           IF HOST-OUT-RETCODE NOT = RC-OK
               GO TO ADDL-EXIT
           END-IF
           IF WS-LOG-CNT >= WS-LOG-MAX
               GO TO ADDL-EXIT
           END-IF
           ADD 1 TO WS-LOG-CNT
           MOVE HOST-IN-TXN-ID     TO WS-LOG-TXN-ID(WS-LOG-CNT)
           MOVE HOST-OUT-OUTCOME   TO WS-LOG-OUTCOME(WS-LOG-CNT)
           MOVE HOST-OUT-ERROR-CODE TO WS-LOG-ERROR-CODE(WS-LOG-CNT).
       ADDL-EXIT.
           EXIT.

       FIND-LOG SECTION.
       FINDL-START.
           MOVE ZERO TO WS-FOUND
           IF HOST-IN-TXN-ID = SPACES
               GO TO FINDL-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LOG-CNT
               IF WS-LOG-TXN-ID(WS-I) = HOST-IN-TXN-ID
                   MOVE WS-I TO WS-FOUND
               END-IF
           END-PERFORM.
       FINDL-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 記帳済みの取引に対する応答を組み立て直す。元帳は触らない。
      * 現在値は照会で取り直せるので、ここでは結果だけを返す。
      *----------------------------------------------------------------
       REPLAY-LOG SECTION.
       REPL-START.
           MOVE WS-LOG-OUTCOME(WS-FOUND)    TO HOST-OUT-OUTCOME
           MOVE WS-LOG-ERROR-CODE(WS-FOUND) TO HOST-OUT-ERROR-CODE
           IF HOST-OC-SUCCESS
               MOVE RC-OK TO HOST-OUT-RETCODE
           ELSE
               MOVE RC-BUSINESS-ERROR TO HOST-OUT-RETCODE
           END-IF.
       REPL-EXIT.
           EXIT.

       CLOSE-LEDGER SECTION.
       CLOSE-L-START.
           IF WS-OPENED = 'Y'
               CLOSE ACCT-FILE
               MOVE 'N' TO WS-OPENED
           END-IF.
       CLOSE-L-EXIT.
           EXIT.

       END PROGRAM ATMHOST.
