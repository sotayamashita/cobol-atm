      *****************************************************************
      * CASHREC.cpy - 現金カセット在庫 (索引編成 1 レコード / ATM 1 台)
      *   レコード長 120 バイト固定
      *   カセットは金種降順で定義すること (払出アルゴリズムの前提)
      *****************************************************************
       01  CASH-RECORD.
           05  CASH-ATM-ID             PIC X(08).
           05  CASH-BUSINESS-DATE      PIC 9(08).
           05  CASH-CASSETTE OCCURS 4 TIMES.
               10  CASH-DENOM          PIC 9(06).
               10  CASH-NOTE-CNT       PIC 9(05).
               10  CASH-LOW-WATER      PIC 9(05).
               10  CASH-STATUS         PIC X(01).
                   88  CASH-ST-OK              VALUE 'O'.
                   88  CASH-ST-LOW             VALUE 'L'.
                   88  CASH-ST-EMPTY           VALUE 'E'.
                   88  CASH-ST-FAULT           VALUE 'F'.
           05  CASH-DISPENSED-TODAY    PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  CASH-DEPOSITED-TODAY    PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  FILLER                  PIC X(04).
