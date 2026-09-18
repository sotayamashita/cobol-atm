      *****************************************************************
      * JRNLREC.cpy - 電子ジャーナル (順編成 / 追記のみ・更新禁止)
      *   レコード長 200 バイト固定
      *   1 取引につき最低 2 レコード (START / END) を出力する。
      *   END が無い取引 = 途中障害。日次バッチで補正対象として抽出する。
      *
      *   フェーズ・結果区分の 88 は呼出インタフェース JRNLIF.cpy に、
      *   取引種別の 88 はセッション ATMSESS.cpy に置く。値の意味を
      *   決めるのは呼出元であり、記録先のレイアウトではないため。
      *****************************************************************
       01  JRNL-RECORD.
           05  JRNL-SEQ                PIC 9(09).
           05  JRNL-TIMESTAMP          PIC 9(14).
           05  JRNL-ATM-ID             PIC X(08).
           05  JRNL-SESSION-ID         PIC X(12).
           05  JRNL-TXN-ID             PIC X(12).
           05  JRNL-PHASE              PIC X(01).
           05  JRNL-TXN-TYPE           PIC X(02).
           05  JRNL-PAN-MASKED         PIC X(16).
           05  JRNL-ACCT-NO            PIC X(10).
           05  JRNL-CPTY-ACCT-NO       PIC X(10).
           05  JRNL-AMOUNT             PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  JRNL-FEE                PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  JRNL-BAL-BEFORE         PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  JRNL-BAL-AFTER          PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  JRNL-RESULT             PIC X(01).
           05  JRNL-ERROR-CODE         PIC X(04).
           05  JRNL-DISPENSED.
               10  JRNL-DSP-CNT OCCURS 4 TIMES PIC 9(03).
           05  FILLER                  PIC X(25).
