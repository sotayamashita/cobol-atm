      *****************************************************************
      * PROGRAM : ATMACCT
      * PURPOSE : 口座マスタアクセスモジュール
      * DESIGN  :
      *   - 口座マスタへの I/O はこのモジュールに一元化する。上位が
      *     直接 FD を持たないことで、将来のホスト接続 (CICS/DB2 等)
      *     への差し替えをこのモジュールの置換だけで完結させる。
      *   - このモジュールは純粋な I/O 層であり、業務判定を持たない。
      *     口座状態 (解約・凍結・休眠) の判定は ATMPOST の検証順序の
      *     中で行う。READ の副作用として業務エラーを返すと、検証順序
      *     を ATMPOST に固定するという設計が成立しなくなるため。
      *   - READLOCK は WITH LOCK 付き READ。UPDATE 完了または UNLOCK
      *     まで他プロセスからの更新を排除する。
      *   - UPDATE は楽観ロックも併用する。呼出元が保持していた
      *     ACCT-VERSION と現物が不一致なら RC-BUSINESS-ERROR を返す。
      *     (悲観ロックが効かない環境への保険。二重防御)
      *   - UNLOCK は取引単位、CLOSE は端末単位。取引の異常終了で
      *     ファイルごと閉じると次取引で索引の再オープンが要る。
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMACCT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-FILE ASSIGN TO 'data/atmacct.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-NO
               LOCK MODE IS MANUAL
               FILE STATUS IS WS-ACCT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-FILE.
       COPY 'ACCTREC.cpy'.

       WORKING-STORAGE SECTION.
       01  WS-ACCT-STATUS              PIC X(02) VALUE '00'.
       01  WS-OPENED                   PIC X(01) VALUE 'N'.
       01  WS-SAVED-VERSION            PIC 9(09) VALUE ZERO.

      *    -- 呼出元が返してきたレコードから版数を取り出すための第 2 像。
      *    -- レイアウトはコピー句から得るので、項目追加でずれない。
       COPY 'ACCTREC.cpy' REPLACING LEADING ==ACCT-== BY ==CHK-==.

       COPY 'RETCODE.cpy'.

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
               WHEN ACCT-FN-UNLOCK  PERFORM UNLOCK-ACCT
               WHEN ACCT-FN-CLOSE   PERFORM CLOSE-ACCT
               WHEN OTHER
                   MOVE RC-FATAL TO ACCT-OUT-RETCODE
           END-EVALUATE
           GOBACK.

       OPEN-ACCT SECTION.
       OPEN-A-START.
           IF WS-OPENED = 'Y'
               GO TO OPEN-A-EXIT
           END-IF
           OPEN I-O ACCT-FILE
           IF WS-ACCT-STATUS = '00'
               MOVE 'Y' TO WS-OPENED
           ELSE
               MOVE RC-IO-ERROR  TO ACCT-OUT-RETCODE
               MOVE EC-SYSTEM-IO TO ACCT-OUT-ERROR-CODE
           END-IF.
       OPEN-A-EXIT.
           EXIT.

       READ-ACCT SECTION.
       READ-A-START.
           PERFORM OPEN-ACCT
           IF ACCT-OUT-RETCODE NOT = RC-OK
               GO TO READ-A-EXIT
           END-IF

           MOVE ACCT-IN-ACCT-NO TO ACCT-NO
           READ ACCT-FILE
               INVALID KEY
                   MOVE RC-NOTFOUND     TO ACCT-OUT-RETCODE
                   MOVE EC-ACCT-UNKNOWN TO ACCT-OUT-ERROR-CODE
               NOT INVALID KEY
                   PERFORM PUBLISH-RECORD
           END-READ.
       READ-A-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * READLOCK : 更新を前提とした排他読み
      *----------------------------------------------------------------
       READ-ACCT-LOCK SECTION.
       READ-AL-START.
           PERFORM OPEN-ACCT
           IF ACCT-OUT-RETCODE NOT = RC-OK
               GO TO READ-AL-EXIT
           END-IF

           MOVE ACCT-IN-ACCT-NO TO ACCT-NO
           READ ACCT-FILE WITH LOCK
               INVALID KEY
                   MOVE RC-NOTFOUND     TO ACCT-OUT-RETCODE
                   MOVE EC-ACCT-UNKNOWN TO ACCT-OUT-ERROR-CODE
               NOT INVALID KEY
                   PERFORM PUBLISH-RECORD
           END-READ

      *    -- ロック競合 (他端末が同一口座を処理中)
           IF WS-ACCT-STATUS = '9D' OR WS-ACCT-STATUS = '99'
               MOVE RC-BUSINESS-ERROR TO ACCT-OUT-RETCODE
               MOVE EC-SYSTEM-BUSY    TO ACCT-OUT-ERROR-CODE
           END-IF.
       READ-AL-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * UPDATE : 楽観ロック検証 + バージョン採番 + REWRITE
      *----------------------------------------------------------------
       UPDATE-ACCT SECTION.
       UPDATE-A-START.
           PERFORM OPEN-ACCT
           IF ACCT-OUT-RETCODE NOT = RC-OK
               GO TO UPDATE-A-EXIT
           END-IF

           MOVE ACCT-IO-RECORD TO CHK-RECORD
           IF CHK-VERSION NOT = WS-SAVED-VERSION
               MOVE RC-BUSINESS-ERROR TO ACCT-OUT-RETCODE
               MOVE EC-SYSTEM-BUSY    TO ACCT-OUT-ERROR-CODE
               GO TO UPDATE-A-EXIT
           END-IF

           MOVE ACCT-IO-RECORD TO ACCT-RECORD
           ADD 1 TO ACCT-VERSION
           MOVE SESS-BUSINESS-DATE TO ACCT-LAST-TXN-DATE

           REWRITE ACCT-RECORD
               INVALID KEY
                   MOVE RC-IO-ERROR  TO ACCT-OUT-RETCODE
                   MOVE EC-SYSTEM-IO TO ACCT-OUT-ERROR-CODE
               NOT INVALID KEY
                   PERFORM PUBLISH-RECORD
           END-REWRITE

           PERFORM UNLOCK-ACCT.
       UPDATE-A-EXIT.
           EXIT.

       UNLOCK-ACCT SECTION.
       UNLOCK-A-START.
           IF WS-OPENED = 'Y'
               UNLOCK ACCT-FILE RECORDS
           END-IF.
       UNLOCK-A-EXIT.
           EXIT.

       CLOSE-ACCT SECTION.
       CLOSE-A-START.
           IF WS-OPENED = 'Y'
               UNLOCK ACCT-FILE RECORDS
               CLOSE ACCT-FILE
               MOVE 'N' TO WS-OPENED
           END-IF.
       CLOSE-A-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 共通: 読んだレコードを呼出元へ渡し、楽観ロックの基準を控える
      *----------------------------------------------------------------
       PUBLISH-RECORD SECTION.
       PUB-START.
           MOVE ACCT-RECORD  TO ACCT-IO-RECORD
           MOVE ACCT-VERSION TO WS-SAVED-VERSION.
       PUB-EXIT.
           EXIT.

       END PROGRAM ATMACCT.
