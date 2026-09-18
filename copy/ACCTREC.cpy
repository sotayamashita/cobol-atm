      *****************************************************************
      * ACCTREC.cpy - 口座マスタ (索引編成 / 主キー ACCT-NO)
      *   レコード長 160 バイト固定
      *   残高は「現在残高 (LEDGER)」と「支払可能残高 (AVAILABLE)」を
      *   分離して保持する。未決済入金や保留額は両者の差として表現する。
      *****************************************************************
       01  ACCT-RECORD.
           05  ACCT-NO                 PIC X(10).
           05  ACCT-BRANCH-CD          PIC X(03).
           05  ACCT-TYPE               PIC X(01).
               88  ACCT-TP-SAVINGS             VALUE 'S'.
               88  ACCT-TP-CHECKING            VALUE 'C'.
               88  ACCT-TP-TIME-DEPOSIT        VALUE 'T'.
           05  ACCT-CURRENCY           PIC X(03).
           05  ACCT-HOLDER-NAME        PIC X(30).
           05  ACCT-STATUS             PIC X(01).
               88  ACCT-ST-NORMAL              VALUE 'N'.
               88  ACCT-ST-FROZEN              VALUE 'F'.
               88  ACCT-ST-DORMANT             VALUE 'D'.
               88  ACCT-ST-CLOSED              VALUE 'X'.
           05  ACCT-LEDGER-BAL         PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  ACCT-AVAILABLE-BAL      PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  ACCT-HOLD-AMT           PIC S9(13)V99 SIGN LEADING SEPARATE.
      *    -- 当座貸越枠 (普通預金は通常ゼロ)
           05  ACCT-OVERDRAFT-LIMIT    PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  ACCT-LAST-TXN-DATE      PIC 9(08).
      *    -- 楽観ロック用。更新のたびに +1 する
           05  ACCT-VERSION            PIC 9(09).
           05  FILLER                  PIC X(31).
