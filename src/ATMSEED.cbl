      *****************************************************************
      * PROGRAM : ATMSEED
      * PURPOSE : 試験用マスタ (口座・カード・現金カセット) の初期作成
      * DESIGN  :
      *   本番では運用部門のバッチが担う領域。ここでは検証シナリオを
      *   再現できる最小限のデータを作る。
      *   PIN ハッシュは ATMAUTH の HASHPIN 機能に算出させる。式を
      *   複製しないことで、HSM 置換時もこのプログラムは無変更で済む。
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMSEED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-FILE ASSIGN TO 'data/atmacct.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ACCT-NO
               FILE STATUS IS WS-STATUS.
           SELECT CARD-FILE ASSIGN TO 'data/atmcard.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CARD-PAN
               FILE STATUS IS WS-STATUS.
           SELECT CASH-FILE ASSIGN TO 'data/atmcash.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CASH-ATM-ID
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-FILE.
       COPY 'ACCTREC.cpy'.
       FD  CARD-FILE.
       COPY 'CARDREC.cpy'.
       FD  CASH-FILE.
       COPY 'CASHREC.cpy'.

       WORKING-STORAGE SECTION.
       01  WS-STATUS                   PIC X(02) VALUE '00'.
       01  WS-PIN-NUM                  PIC 9(04) VALUE ZERO.

       COPY 'ATMCONST.cpy'.
       COPY 'AUTHIF.cpy'.
       COPY 'ATMSESS.cpy'.

       PROCEDURE DIVISION.

       MAIN-CONTROL SECTION.
       MAIN-START.
           OPEN OUTPUT ACCT-FILE CARD-FILE CASH-FILE

           PERFORM SEED-ACCOUNTS
           PERFORM SEED-CARDS
           PERFORM SEED-CASSETTE

           CLOSE ACCT-FILE CARD-FILE CASH-FILE
           DISPLAY 'マスタを初期化しました。'
           DISPLAY '  口座 1000000001 / カード 4900123456780001 / PIN 1234'
           DISPLAY '  口座 1000000002 / カード 4900123456780002 / PIN 9999'
           STOP RUN.

       SEED-ACCOUNTS SECTION.
       SA-START.
      *    -- 通常口座 (残高 350,000 円)
           MOVE SPACES TO ACCT-RECORD
           MOVE '1000000001' TO ACCT-NO
           MOVE CN-BRANCH-CD TO ACCT-BRANCH-CD
           SET  ACCT-TP-SAVINGS TO TRUE
           MOVE 'JPY'        TO ACCT-CURRENCY
           MOVE '山田 太郎'   TO ACCT-HOLDER-NAME
           SET  ACCT-ST-NORMAL TO TRUE
           MOVE 350000.00    TO ACCT-LEDGER-BAL
           MOVE 350000.00    TO ACCT-AVAILABLE-BAL
           MOVE ZERO         TO ACCT-HOLD-AMT
           MOVE ZERO         TO ACCT-OVERDRAFT-LIMIT
           MOVE 20260917     TO ACCT-LAST-TXN-DATE
           MOVE 1            TO ACCT-VERSION
           WRITE ACCT-RECORD END-WRITE

      *    -- 振替先口座 (当座貸越枠 100,000 円あり)
           MOVE SPACES TO ACCT-RECORD
           MOVE '1000000002' TO ACCT-NO
           MOVE CN-BRANCH-CD TO ACCT-BRANCH-CD
           SET  ACCT-TP-CHECKING TO TRUE
           MOVE 'JPY'        TO ACCT-CURRENCY
           MOVE '鈴木 花子'   TO ACCT-HOLDER-NAME
           SET  ACCT-ST-NORMAL TO TRUE
           MOVE 80000.00     TO ACCT-LEDGER-BAL
           MOVE 80000.00     TO ACCT-AVAILABLE-BAL
           MOVE ZERO         TO ACCT-HOLD-AMT
           MOVE 100000.00    TO ACCT-OVERDRAFT-LIMIT
           MOVE 20260917     TO ACCT-LAST-TXN-DATE
           MOVE 1            TO ACCT-VERSION
           WRITE ACCT-RECORD END-WRITE

      *    -- 凍結口座 (エラー系の検証用)
           MOVE SPACES TO ACCT-RECORD
           MOVE '1000000003' TO ACCT-NO
           MOVE CN-BRANCH-CD TO ACCT-BRANCH-CD
           SET  ACCT-TP-SAVINGS TO TRUE
           MOVE 'JPY'        TO ACCT-CURRENCY
           MOVE '佐藤 一郎'   TO ACCT-HOLDER-NAME
           SET  ACCT-ST-FROZEN TO TRUE
           MOVE 500000.00    TO ACCT-LEDGER-BAL
           MOVE 500000.00    TO ACCT-AVAILABLE-BAL
           MOVE ZERO         TO ACCT-HOLD-AMT
           MOVE ZERO         TO ACCT-OVERDRAFT-LIMIT
           MOVE 20260917     TO ACCT-LAST-TXN-DATE
           MOVE 1            TO ACCT-VERSION
           WRITE ACCT-RECORD END-WRITE.
       SA-EXIT.
           EXIT.

       SEED-CARDS SECTION.
       SC-START.
           MOVE SPACES TO CARD-RECORD
           MOVE '4900123456780001' TO CARD-PAN
           MOVE '1000000001'       TO CARD-ACCT-NO
           MOVE '山田 太郎'         TO CARD-HOLDER-NAME
           MOVE 202812             TO CARD-EXPIRY-YYYYMM
           MOVE 12345678           TO CARD-PIN-SALT
           MOVE 1234               TO WS-PIN-NUM
           PERFORM CALC-HASH
           SET  CARD-ST-ACTIVE TO TRUE
           MOVE ZERO     TO CARD-PIN-FAIL-CNT
           MOVE ZERO     TO CARD-LAST-USED-DATE
           MOVE ZERO     TO CARD-DAILY-DATE
           MOVE ZERO     TO CARD-DAILY-WD-AMT
           MOVE ZERO     TO CARD-DAILY-WD-CNT
           MOVE 200000.00 TO CARD-LIMIT-PER-TXN
           MOVE 500000.00 TO CARD-LIMIT-DAILY-AMT
           MOVE 10       TO CARD-LIMIT-DAILY-CNT
           WRITE CARD-RECORD END-WRITE

           MOVE SPACES TO CARD-RECORD
           MOVE '4900123456780002' TO CARD-PAN
           MOVE '1000000002'       TO CARD-ACCT-NO
           MOVE '鈴木 花子'         TO CARD-HOLDER-NAME
           MOVE 202703             TO CARD-EXPIRY-YYYYMM
           MOVE 87654321           TO CARD-PIN-SALT
           MOVE 9999               TO WS-PIN-NUM
           PERFORM CALC-HASH
           SET  CARD-ST-ACTIVE TO TRUE
           MOVE ZERO     TO CARD-PIN-FAIL-CNT
           MOVE ZERO     TO CARD-LAST-USED-DATE
           MOVE ZERO     TO CARD-DAILY-DATE
           MOVE ZERO     TO CARD-DAILY-WD-AMT
           MOVE ZERO     TO CARD-DAILY-WD-CNT
           MOVE 50000.00 TO CARD-LIMIT-PER-TXN
           MOVE 100000.00 TO CARD-LIMIT-DAILY-AMT
           MOVE 3        TO CARD-LIMIT-DAILY-CNT
           WRITE CARD-RECORD END-WRITE.
       SC-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * ハッシュ式の所有者は ATMAUTH。ここでは呼ぶだけ。
      *----------------------------------------------------------------
       CALC-HASH SECTION.
       CH-START.
           SET AUTH-FN-HASH-PIN TO TRUE
           MOVE CARD-PIN-SALT TO AUTH-IN-SALT
           MOVE WS-PIN-NUM    TO AUTH-IN-PIN
           CALL 'ATMAUTH' USING AUTH-PARM ATM-SESSION
           MOVE AUTH-OUT-HASH TO CARD-PIN-HASH.
       CH-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * カセットは金種降順。ATMCASH の DP はこの順序に依存しないが、
      * 払出明細の表示順を自然にするため降順で定義する。
      * 2,000 円券は意図的に少枚数とし、金種不足系の検証を可能にする。
      *----------------------------------------------------------------
       SEED-CASSETTE SECTION.
       SD-START.
           MOVE SPACES TO CASH-RECORD
           MOVE CN-ATM-ID TO CASH-ATM-ID
           MOVE 20260917   TO CASH-BUSINESS-DATE

           MOVE 10000 TO CASH-DENOM(1)
           MOVE 300   TO CASH-NOTE-CNT(1)
           MOVE 20    TO CASH-LOW-WATER(1)
           MOVE 'O'   TO CASH-STATUS(1)

           MOVE 5000  TO CASH-DENOM(2)
           MOVE 200   TO CASH-NOTE-CNT(2)
           MOVE 20    TO CASH-LOW-WATER(2)
           MOVE 'O'   TO CASH-STATUS(2)

           MOVE 2000  TO CASH-DENOM(3)
           MOVE 5     TO CASH-NOTE-CNT(3)
           MOVE 10    TO CASH-LOW-WATER(3)
           MOVE 'L'   TO CASH-STATUS(3)

           MOVE 1000  TO CASH-DENOM(4)
           MOVE 400   TO CASH-NOTE-CNT(4)
           MOVE 50    TO CASH-LOW-WATER(4)
           MOVE 'O'   TO CASH-STATUS(4)

           MOVE ZERO TO CASH-DISPENSED-TODAY
           MOVE ZERO TO CASH-DEPOSITED-TODAY
           WRITE CASH-RECORD END-WRITE.
       SD-EXIT.
           EXIT.

       END PROGRAM ATMSEED.
