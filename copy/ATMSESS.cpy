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
      *    -- カード媒体。磁気は JIS II 型 (国内独自)、IC は全銀協
      *    -- IC キャッシュカード標準仕様 (EMV 準拠)。両者は併存する。
           05  SESS-CARD-MEDIA         PIC X(01).
               88  SESS-MEDIA-MAGNETIC         VALUE 'M'.
               88  SESS-MEDIA-IC               VALUE 'I'.
      *    -- 発行区分。自行カードと提携行カードで手数料体系が違う。
      *    -- カードマスタを読めるのは ATMAUTH だけなので、認証時に
      *    -- ここへ載せて ATMPOST の手数料計算へ渡す。
           05  SESS-CARD-KIND          PIC X(01).
               88  SESS-CK-OWN                 VALUE 'O'.
               88  SESS-CK-PARTNER             VALUE 'P'.
      *    -- 成立した認証方式。限度額はこれで変わる。
      *       P = 暗証番号のみ (ホスト照合)
      *       O = IC オフライン PIN (カード内で照合)
      *       B = IC + 生体認証
           05  SESS-AUTH-METHOD        PIC X(01).
               88  SESS-AM-PIN-ONLY            VALUE 'P'.
               88  SESS-AM-IC-OFFLINE          VALUE 'O'.
               88  SESS-AM-BIOMETRIC           VALUE 'B'.
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
           05  SESS-CPTY-BANK-CD       PIC X(04).
           05  SESS-CPTY-ACCT-NO       PIC X(10).
      *    -- 当日の曜日区分。取引の属性なので、営業日・時刻を確定する
      *    -- のと同じ場所 (ATMMAIN の取引開始) で一度だけ決める。
      *    -- 手数料も全銀接続も下位は読むだけ。
           05  SESS-DAY-TYPE           PIC X(01).
               88  SESS-DT-WEEKDAY             VALUE 'W'.
               88  SESS-DT-SATURDAY            VALUE 'S'.
               88  SESS-DT-HOLIDAY             VALUE 'H'.
           05  SESS-BAL-BEFORE         PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  SESS-BAL-AFTER          PIC S9(13)V99 SIGN LEADING SEPARATE.
      *    -- 他行あて為替の追跡番号と入金日。EJ と画面が同じ値を見る。
           05  SESS-TRACE-NO           PIC X(12).
           05  SESS-VALUE-DATE         PIC 9(08).
      *    -- 直近の結果 (EJ への出力と画面表示に使う)
           05  SESS-ERROR-CODE         PIC X(04).
           05  SESS-ERROR-MESSAGE      PIC X(60).
      *    -- 払出金種 (ATMCASH が設定)
           05  SESS-DISPENSE.
               10  SESS-DSP-DENOM OCCURS 4 TIMES PIC 9(06).
               10  SESS-DSP-CNT   OCCURS 4 TIMES PIC 9(03).
