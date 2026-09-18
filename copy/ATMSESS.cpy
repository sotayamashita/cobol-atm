      *****************************************************************
      * ATMSESS.cpy - セッションコンテキスト
      *   ATMMAIN が生成し、全下位モジュールに参照渡しする共有域。
      *   下位モジュールは自モジュールの責務範囲の項目のみ更新すること。
      *****************************************************************
       01  ATM-SESSION.
           05  SESS-ATM-ID             PIC X(08).
           05  SESS-SESSION-ID         PIC X(12).
           05  SESS-BUSINESS-DATE      PIC 9(08).
           05  SESS-TIMESTAMP          PIC 9(14).
      *    -- 認証結果 (ATMAUTH が設定)
           05  SESS-AUTHENTICATED      PIC X(01).
               88  SESS-AUTH-OK                VALUE 'Y'.
               88  SESS-AUTH-NG                VALUE 'N'.
           05  SESS-PAN                PIC X(16).
           05  SESS-PAN-MASKED         PIC X(16).
           05  SESS-ACCT-NO            PIC X(10).
           05  SESS-HOLDER-NAME        PIC X(30).
      *    -- 取引単位の作業域 (ATMMAIN が取引ごとに初期化)
           05  SESS-TXN-ID             PIC X(12).
           05  SESS-TXN-TYPE           PIC X(02).
               88  SESS-TT-INQUIRY             VALUE 'IQ'.
               88  SESS-TT-WITHDRAWAL          VALUE 'WD'.
               88  SESS-TT-DEPOSIT             VALUE 'DP'.
               88  SESS-TT-TRANSFER            VALUE 'TR'.
               88  SESS-TT-PIN-CHANGE          VALUE 'PC'.
           05  SESS-TXN-AMOUNT         PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  SESS-TXN-FEE            PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  SESS-CPTY-ACCT-NO       PIC X(10).
           05  SESS-BAL-BEFORE         PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  SESS-BAL-AFTER          PIC S9(13)V99 SIGN LEADING SEPARATE.
      *    -- 直近の結果 (EJ への出力と画面表示に使う)
           05  SESS-ERROR-CODE         PIC X(04).
           05  SESS-ERROR-MESSAGE      PIC X(60).
      *    -- 払出金種 (ATMCASH が設定)
           05  SESS-DISPENSE.
               10  SESS-DSP-DENOM OCCURS 4 TIMES PIC 9(06).
               10  SESS-DSP-CNT   OCCURS 4 TIMES PIC 9(03).
