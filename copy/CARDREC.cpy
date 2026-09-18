      *****************************************************************
      * CARDREC.cpy - カードマスタ (索引編成 / 主キー CARD-PAN)
      *   レコード長 160 バイト固定
      *   金額は SIGN LEADING SEPARATE の表示形式とし、データファイルを
      *   テキストエディタで検証可能にする (運用時の障害解析を優先)
      *   PIN は平文保持せず、ソルト付きハッシュを保持する
      *****************************************************************
       01  CARD-RECORD.
           05  CARD-PAN                PIC X(16).
           05  CARD-ACCT-NO            PIC X(10).
           05  CARD-HOLDER-NAME        PIC X(30).
           05  CARD-EXPIRY-YYYYMM      PIC 9(06).
           05  CARD-PIN-SALT           PIC 9(08).
           05  CARD-PIN-HASH           PIC 9(12).
           05  CARD-STATUS             PIC X(01).
               88  CARD-ST-ACTIVE              VALUE 'A'.
               88  CARD-ST-LOCKED              VALUE 'L'.
               88  CARD-ST-CAPTURED            VALUE 'C'.
           05  CARD-PIN-FAIL-CNT       PIC 9(01).
           05  CARD-LAST-USED-DATE     PIC 9(08).
      *    -- 当日累計 (日付が変わったら AUTH モジュールがリセット)
           05  CARD-DAILY-DATE         PIC 9(08).
           05  CARD-DAILY-WD-AMT       PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  CARD-DAILY-WD-CNT       PIC 9(03).
      *    -- カード単位の限度額 (口座属性より優先)
           05  CARD-LIMIT-PER-TXN      PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  CARD-LIMIT-DAILY-AMT    PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  CARD-LIMIT-DAILY-CNT    PIC 9(03).
           05  FILLER                  PIC X(06).
