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

       END PROGRAM ATMJRNL.
