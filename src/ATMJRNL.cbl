      *****************************************************************
      * PROGRAM : ATMJRNL
      * PURPOSE : 電子ジャーナル (EJ) 出力モジュール
      * DESIGN  :
      *   - EJ は追記専用。いかなる場合も既存レコードを更新・削除しない。
      *   - OPEN 時に既存 EJ を走査して最大シーケンス番号を得る。これは
      *     電源断からの再起動時にシーケンスを継続させるための処置。
      *   - WRITE はレコードを組み立てて即時フラッシュする。取引成立前に
      *     START を、成立後に END を書くことで、END 欠落レコードを
      *     日次バッチが「不確定取引」として検出できる。
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMJRNL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JRNL-FILE ASSIGN TO 'data/atmjrnl.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-JRNL-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  JRNL-FILE.
       COPY 'JRNLREC.cpy'.

       WORKING-STORAGE SECTION.
       01  WS-JRNL-STATUS              PIC X(02) VALUE '00'.
       01  WS-SEQ                      PIC 9(09) VALUE ZERO.
       01  WS-OPENED                   PIC X(01) VALUE 'N'.
      *    -- 走査用の状態。追記用 (WS-OPENED) とは別に持つ。同じ FD を
      *    -- EXTEND と INPUT で同時に開けないため、状態を共用すると
      *    -- どちらのモードで開いているか判別できなくなる。
       01  WS-SCANNING                 PIC X(01) VALUE 'N'.
       01  WS-IDX                      PIC 9(02) VALUE ZERO.
       COPY 'ATMCONST.cpy'.
       COPY 'RETCODE.cpy'.

       LINKAGE SECTION.
       COPY 'JRNLIF.cpy'.
       COPY 'ATMSESS.cpy'.

       PROCEDURE DIVISION USING JRNL-PARM ATM-SESSION.

       MAIN-CONTROL SECTION.
       MAIN-START.
           MOVE RC-OK TO JRNL-OUT-RETCODE
           EVALUATE TRUE
               WHEN JRNL-FN-OPEN    PERFORM OPEN-JOURNAL
               WHEN JRNL-FN-WRITE   PERFORM WRITE-JOURNAL
               WHEN JRNL-FN-CLOSE   PERFORM CLOSE-JOURNAL
               WHEN JRNL-FN-SCAN-OPEN   PERFORM SCAN-OPEN-JOURNAL
               WHEN JRNL-FN-SCAN-NEXT   PERFORM SCAN-NEXT-JOURNAL
               WHEN JRNL-FN-SCAN-CLOSE  PERFORM SCAN-CLOSE-JOURNAL
               WHEN OTHER           MOVE RC-FATAL TO JRNL-OUT-RETCODE
           END-EVALUATE
           GOBACK.

      *----------------------------------------------------------------
      * OPEN : 既存 EJ を走査して最終シーケンスを復元する
      *----------------------------------------------------------------
       OPEN-JOURNAL SECTION.
       OPEN-J-START.
           IF WS-OPENED = 'Y'
               GO TO OPEN-J-EXIT
           END-IF

      *    -- 走査中は同じ FD が INPUT で開いている。ここで EXTEND を
      *    -- 重ねると走査が壊れるので、呼出順序の誤りとして弾く。
           IF WS-SCANNING = 'Y'
               MOVE RC-FATAL TO JRNL-OUT-RETCODE
               GO TO OPEN-J-EXIT
           END-IF

           MOVE ZERO TO WS-SEQ
           OPEN INPUT JRNL-FILE
           IF WS-JRNL-STATUS = '00' OR '05'
               PERFORM UNTIL WS-JRNL-STATUS NOT = '00'
                   READ JRNL-FILE
                       AT END
                           EXIT PERFORM
                       NOT AT END
                           IF JRNL-SEQ > WS-SEQ
                               MOVE JRNL-SEQ TO WS-SEQ
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE JRNL-FILE
           END-IF

           OPEN EXTEND JRNL-FILE
      *    -- 初回起動時は EJ が存在しないため新規作成する
           IF WS-JRNL-STATUS = '35'
               OPEN OUTPUT JRNL-FILE
           END-IF
           IF WS-JRNL-STATUS NOT = '00' AND WS-JRNL-STATUS NOT = '05'
               MOVE RC-IO-ERROR TO JRNL-OUT-RETCODE
           ELSE
               MOVE 'Y' TO WS-OPENED
           END-IF.
       OPEN-J-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * WRITE : セッション文脈から 1 レコード組み立てて追記
      *----------------------------------------------------------------
       WRITE-JOURNAL SECTION.
       WRITE-J-START.
           IF WS-OPENED NOT = 'Y'
               PERFORM OPEN-JOURNAL
               IF JRNL-OUT-RETCODE NOT = RC-OK
                   GO TO WRITE-J-EXIT
               END-IF
           END-IF

           MOVE SPACES TO JRNL-RECORD
           ADD 1 TO WS-SEQ
           MOVE WS-SEQ                 TO JRNL-SEQ
           MOVE SESS-TIMESTAMP         TO JRNL-TIMESTAMP
           MOVE SESS-ATM-ID            TO JRNL-ATM-ID
           MOVE SESS-SESSION-ID        TO JRNL-SESSION-ID
           MOVE SESS-TXN-ID            TO JRNL-TXN-ID
           MOVE JRNL-IN-PHASE          TO JRNL-PHASE
           MOVE SESS-TXN-TYPE          TO JRNL-TXN-TYPE
           MOVE SESS-PAN-MASKED        TO JRNL-PAN-MASKED
           MOVE SESS-ACCT-NO           TO JRNL-ACCT-NO
           MOVE SESS-CPTY-ACCT-NO      TO JRNL-CPTY-ACCT-NO
           MOVE SESS-TXN-AMOUNT        TO JRNL-AMOUNT
           MOVE SESS-TXN-FEE           TO JRNL-FEE
           MOVE SESS-BAL-BEFORE        TO JRNL-BAL-BEFORE
           MOVE SESS-BAL-AFTER         TO JRNL-BAL-AFTER
           MOVE JRNL-IN-RESULT         TO JRNL-RESULT
           MOVE SESS-ERROR-CODE        TO JRNL-ERROR-CODE
           MOVE SESS-TRACE-NO          TO JRNL-TRACE-NO

           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > CN-CASSETTE-CNT
               MOVE SESS-DSP-CNT(WS-IDX) TO JRNL-DSP-CNT(WS-IDX)
           END-PERFORM

           WRITE JRNL-RECORD
           IF WS-JRNL-STATUS NOT = '00'
               MOVE RC-IO-ERROR TO JRNL-OUT-RETCODE
           END-IF.
       WRITE-J-EXIT.
           EXIT.

       CLOSE-JOURNAL SECTION.
       CLOSE-J-START.
           IF WS-OPENED = 'Y'
               CLOSE JRNL-FILE
               MOVE 'N' TO WS-OPENED
           END-IF.
       CLOSE-J-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * SCANOPEN : 締めバッチが EJ を先頭から読むために開く
      *   走査中に追記が起きるとファイルが伸び続けて終端が定まらない
      *   ため、追記用に開いていれば先に閉じて追記経路を塞ぐ。締め
      *   バッチは端末停止中に走るので、両方が同時に必要になることは
      *   ない。WS-SEQ は保持したままなので、走査後に再度 OPEN すれば
      *   シーケンスはそのまま継続できる。
      *----------------------------------------------------------------
       SCAN-OPEN-JOURNAL SECTION.
       SCAN-O-START.
           IF WS-SCANNING = 'Y'
               GO TO SCAN-O-EXIT
           END-IF

           PERFORM CLOSE-JOURNAL

           MOVE 'N' TO JRNL-OUT-EOF
           MOVE SPACES TO JRNL-OUT-RECORD
           OPEN INPUT JRNL-FILE
      *    -- EJ 未作成 (35) は「走査対象 0 件」であり障害ではない。
      *    -- 初日の締めを異常終了させないため即 EOF として扱う。
           EVALUATE TRUE
               WHEN WS-JRNL-STATUS = '00' OR WS-JRNL-STATUS = '05'
                   MOVE 'Y' TO WS-SCANNING
               WHEN WS-JRNL-STATUS = '35'
                   MOVE 'Y' TO JRNL-OUT-EOF
               WHEN OTHER
                   MOVE RC-IO-ERROR TO JRNL-OUT-RETCODE
           END-EVALUATE.
       SCAN-O-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * SCANNEXT : 1 レコード読んで呼出元に返す
      *----------------------------------------------------------------
       SCAN-NEXT-JOURNAL SECTION.
       SCAN-N-START.
           MOVE SPACES TO JRNL-OUT-RECORD
      *    -- 未オープン / EJ 無しの場合も EOF を返すだけにする。
      *    -- 呼出元は EOF 判定だけでループを終えられる。
           IF WS-SCANNING NOT = 'Y'
               MOVE 'Y' TO JRNL-OUT-EOF
               GO TO SCAN-N-EXIT
           END-IF

           MOVE 'N' TO JRNL-OUT-EOF
           READ JRNL-FILE
               AT END
                   MOVE 'Y' TO JRNL-OUT-EOF
               NOT AT END
                   MOVE JRNL-RECORD TO JRNL-OUT-RECORD
           END-READ

           IF JRNL-OUT-EOF NOT = 'Y' AND WS-JRNL-STATUS NOT = '00'
               MOVE RC-IO-ERROR TO JRNL-OUT-RETCODE
           END-IF.
       SCAN-N-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * SCANCLOS : 走査を終える。追記用の再オープンは行わない
      *   (再開の要否を決めるのは呼出元であり、ここで勝手に EXTEND で
      *    開くと締め処理中に EJ が伸びうるため)
      *----------------------------------------------------------------
       SCAN-CLOSE-JOURNAL SECTION.
       SCAN-C-START.
           IF WS-SCANNING = 'Y'
               CLOSE JRNL-FILE
               MOVE 'N' TO WS-SCANNING
           END-IF
           MOVE 'Y' TO JRNL-OUT-EOF
           MOVE SPACES TO JRNL-OUT-RECORD.
       SCAN-C-EXIT.
           EXIT.

       END PROGRAM ATMJRNL.
