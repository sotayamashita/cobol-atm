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
           SELECT BANK-FILE ASSIGN TO 'data/atmbank.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS BANK-CD
               FILE STATUS IS WS-STATUS.
           SELECT FEE-FILE ASSIGN TO 'data/atmfee.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT LIMIT-FILE ASSIGN TO 'data/atmlimit.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT HOL-FILE ASSIGN TO 'data/atmhol.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT HOUR-FILE ASSIGN TO 'data/atmhour.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.
           SELECT CLOSE-FILE ASSIGN TO 'data/atmclose.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CLS-ATM-ID
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-FILE.
       COPY 'ACCTREC.cpy'.
       FD  CARD-FILE.
       COPY 'CARDREC.cpy'.
       FD  CASH-FILE.
       COPY 'CASHREC.cpy'.
       FD  BANK-FILE.
       COPY 'BANKREC.cpy'.
       FD  FEE-FILE.
       COPY 'FEEREC.cpy'.
       FD  LIMIT-FILE.
       COPY 'LIMITREC.cpy'.
       FD  HOL-FILE.
       01  HOL-RECORD.
           05  HOL-DATE                PIC 9(08).
           05  FILLER                  PIC X(32).
       FD  HOUR-FILE.
       COPY 'HOURREC.cpy'.
       FD  CLOSE-FILE.
       COPY 'CLOSEREC.cpy'.

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
                       BANK-FILE FEE-FILE LIMIT-FILE HOL-FILE
                       HOUR-FILE CLOSE-FILE

           PERFORM SEED-ACCOUNTS
           PERFORM SEED-CARDS
           PERFORM SEED-CASSETTE
           PERFORM SEED-BANKS
           PERFORM SEED-FEES
           PERFORM SEED-LIMITS
           PERFORM SEED-HOLIDAYS
           PERFORM SEED-HOURS
           PERFORM SEED-CLOSE-STATE

           CLOSE ACCT-FILE CARD-FILE CASH-FILE
                 BANK-FILE FEE-FILE LIMIT-FILE HOL-FILE
                 HOUR-FILE CLOSE-FILE
           DISPLAY 'マスタを初期化しました。'
           DISPLAY '  口座 1000000001 / カード 4900123456780001'
                   ' / PIN 1234 / 磁気'
           DISPLAY '  口座 1000000002 / カード 4900123456780002'
                   ' / PIN 9999 / IC + 生体認証'
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
           SET  CARD-MD-MAGNETIC TO TRUE
           SET  CARD-BIO-NO      TO TRUE
           SET  CARD-KD-OWN      TO TRUE
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
           SET  CARD-MD-IC   TO TRUE
           SET  CARD-BIO-YES TO TRUE
           SET  CARD-KD-OWN  TO TRUE
           WRITE CARD-RECORD END-WRITE.
       SC-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 金融機関マスタ。自行 1 行 + 他行 3 行。
      * モアタイム参加 / 未参加 / 時間限定参加 の 3 パターンを揃える。
      *----------------------------------------------------------------
       SEED-BANKS SECTION.
       SB-START.
      *    -- 自行
           MOVE SPACES TO BANK-RECORD
           MOVE '0001'        TO BANK-CD
           MOVE 'コボル銀行'   TO BANK-NAME
           SET  BANK-OWN           TO TRUE
           SET  BANK-MT-JOINED     TO TRUE
           MOVE 0000 TO BANK-MT-FROM-HHMM
           MOVE 0000 TO BANK-MT-TO-HHMM
           MOVE 'Y'  TO BANK-ONLINE
           WRITE BANK-RECORD END-WRITE

      *    -- モアタイム参加 (24 時間接続)
           MOVE SPACES TO BANK-RECORD
           MOVE '0005'        TO BANK-CD
           MOVE 'さくら信販銀行' TO BANK-NAME
           SET  BANK-OTHER         TO TRUE
           SET  BANK-MT-JOINED     TO TRUE
           MOVE 0000 TO BANK-MT-FROM-HHMM
           MOVE 0000 TO BANK-MT-TO-HHMM
           MOVE 'Y'  TO BANK-ONLINE
           WRITE BANK-RECORD END-WRITE

      *    -- モアタイム未参加。夜間・休日は翌営業日扱いになる
           MOVE SPACES TO BANK-RECORD
           MOVE '0009'        TO BANK-CD
           MOVE 'みなと第一銀行' TO BANK-NAME
           SET  BANK-OTHER         TO TRUE
           SET  BANK-MT-NOT-JOINED TO TRUE
           MOVE 0000 TO BANK-MT-FROM-HHMM
           MOVE 0000 TO BANK-MT-TO-HHMM
           MOVE 'Y'  TO BANK-ONLINE
           WRITE BANK-RECORD END-WRITE

      *    -- 参加しているが接続時間を限定している行
           MOVE SPACES TO BANK-RECORD
           MOVE '0012'        TO BANK-CD
           MOVE '北洋みらい信用金庫' TO BANK-NAME
           SET  BANK-OTHER         TO TRUE
           SET  BANK-MT-JOINED     TO TRUE
           MOVE 0800 TO BANK-MT-FROM-HHMM
           MOVE 2100 TO BANK-MT-TO-HHMM
           MOVE 'Y'  TO BANK-ONLINE
           WRITE BANK-RECORD END-WRITE.
       SB-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 手数料マスタ。曜日区分 × 時間帯 × カード区分。
      * 実際の大手行の体系に合わせた値。
      *   平日 8:45-18:00 無料 / 平日それ以外 110 円
      *   土曜 9:00-14:00 110 円 / 土曜それ以外 220 円
      *   日曜・祝日 220 円
      * 提携行カードは一律 110 円上乗せする。
      *----------------------------------------------------------------
       SEED-FEES SECTION.
       SF-START.
      *    -- 自行カード / 出金
           PERFORM WRITE-FEE-OWN-WEEKDAY
           PERFORM WRITE-FEE-OWN-SATURDAY
           PERFORM WRITE-FEE-OWN-HOLIDAY
           PERFORM WRITE-FEE-PARTNER
           PERFORM WRITE-FEE-TRANSFER.
       SF-EXIT.
           EXIT.

       WRITE-FEE-OWN-WEEKDAY SECTION.
       WFW-START.
           PERFORM SET-FEE-DEFAULTS
           SET  FEE-CK-OWN TO TRUE
           SET  FEE-DT-WEEKDAY TO TRUE
           MOVE 0000 TO FEE-FROM-HHMM
           MOVE 0845 TO FEE-TO-HHMM
           MOVE 110  TO FEE-AMOUNT
           WRITE FEE-RECORD END-WRITE

           MOVE 0845 TO FEE-FROM-HHMM
           MOVE 1800 TO FEE-TO-HHMM
           MOVE ZERO TO FEE-AMOUNT
           WRITE FEE-RECORD END-WRITE

           MOVE 1800 TO FEE-FROM-HHMM
           MOVE 2400 TO FEE-TO-HHMM
           MOVE 110  TO FEE-AMOUNT
           WRITE FEE-RECORD END-WRITE.
       WFW-EXIT.
           EXIT.

       WRITE-FEE-OWN-SATURDAY SECTION.
       WFS-START.
           PERFORM SET-FEE-DEFAULTS
           SET  FEE-CK-OWN TO TRUE
           SET  FEE-DT-SATURDAY TO TRUE
           MOVE 0000 TO FEE-FROM-HHMM
           MOVE 0900 TO FEE-TO-HHMM
           MOVE 220  TO FEE-AMOUNT
           WRITE FEE-RECORD END-WRITE

           MOVE 0900 TO FEE-FROM-HHMM
           MOVE 1400 TO FEE-TO-HHMM
           MOVE 110  TO FEE-AMOUNT
           WRITE FEE-RECORD END-WRITE

           MOVE 1400 TO FEE-FROM-HHMM
           MOVE 2400 TO FEE-TO-HHMM
           MOVE 220  TO FEE-AMOUNT
           WRITE FEE-RECORD END-WRITE.
       WFS-EXIT.
           EXIT.

       WRITE-FEE-OWN-HOLIDAY SECTION.
       WFH-START.
           PERFORM SET-FEE-DEFAULTS
           SET  FEE-CK-OWN TO TRUE
           SET  FEE-DT-HOLIDAY TO TRUE
           MOVE 0000 TO FEE-FROM-HHMM
           MOVE 2400 TO FEE-TO-HHMM
           MOVE 220  TO FEE-AMOUNT
           WRITE FEE-RECORD END-WRITE.
       WFH-EXIT.
           EXIT.

      *    -- 提携行カードは終日 220 円 / 休日は 330 円
       WRITE-FEE-PARTNER SECTION.
       WFP-START.
           PERFORM SET-FEE-DEFAULTS
           SET  FEE-CK-PARTNER TO TRUE
           SET  FEE-DT-WEEKDAY TO TRUE
           MOVE 0000 TO FEE-FROM-HHMM
           MOVE 2400 TO FEE-TO-HHMM
           MOVE 220  TO FEE-AMOUNT
           WRITE FEE-RECORD END-WRITE

           SET  FEE-DT-SATURDAY TO TRUE
           WRITE FEE-RECORD END-WRITE

           SET  FEE-DT-HOLIDAY TO TRUE
           MOVE 330 TO FEE-AMOUNT
           WRITE FEE-RECORD END-WRITE.
       WFP-EXIT.
           EXIT.

      *    -- 振込手数料。出金と違い、時間帯より「自行あて / 他行あて」
      *    -- で大きく変わるのが実態だが、相手行区分は ATMPOST が
      *    -- 知る前に手数料を出す必要があるため、当面は時間帯のみで
      *    -- 持つ。他行あての加算は全銀接続の組込み時に見直す。
       WRITE-FEE-TRANSFER SECTION.
       WFT-START.
           PERFORM SET-FEE-DEFAULTS
           SET  FEE-CK-OWN TO TRUE
           MOVE 'TR' TO FEE-TXN-TYPE

           SET  FEE-DT-WEEKDAY TO TRUE
           MOVE 0000 TO FEE-FROM-HHMM
           MOVE 0845 TO FEE-TO-HHMM
           MOVE 220  TO FEE-AMOUNT
           WRITE FEE-RECORD END-WRITE

           MOVE 0845 TO FEE-FROM-HHMM
           MOVE 1800 TO FEE-TO-HHMM
           MOVE 110  TO FEE-AMOUNT
           WRITE FEE-RECORD END-WRITE

           MOVE 1800 TO FEE-FROM-HHMM
           MOVE 2400 TO FEE-TO-HHMM
           MOVE 220  TO FEE-AMOUNT
           WRITE FEE-RECORD END-WRITE

           SET  FEE-DT-SATURDAY TO TRUE
           MOVE 0000 TO FEE-FROM-HHMM
           MOVE 2400 TO FEE-TO-HHMM
           MOVE 220  TO FEE-AMOUNT
           WRITE FEE-RECORD END-WRITE

           SET  FEE-DT-HOLIDAY TO TRUE
           WRITE FEE-RECORD END-WRITE.
       WFT-EXIT.
           EXIT.

       SET-FEE-DEFAULTS SECTION.
       SFD-START.
           MOVE SPACES TO FEE-RECORD
           MOVE 'WD' TO FEE-TXN-TYPE.
       SFD-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 限度額マスタ。媒体 × 認証方式。
      * ゆうちょ銀行が 2026 年 8 月 17 日に、暗証番号のみの取引の
      * 1 日あたり引出上限を 200 万円から 50 万円へ引き下げた体系に
      * 合わせている。IC + 生体認証なら 500 万円まで。
      *----------------------------------------------------------------
       SEED-LIMITS SECTION.
       SL-START.
      *    -- 磁気カード + 暗証番号のみ
           MOVE SPACES TO LIMIT-RECORD
           MOVE 'M' TO LIMIT-MEDIA
           MOVE 'P' TO LIMIT-AUTH-METHOD
           MOVE 500000     TO LIMIT-PER-TXN
           MOVE 500000     TO LIMIT-DAILY-AMT
           MOVE 10         TO LIMIT-DAILY-CNT
           MOVE '磁気 暗証番号のみ' TO LIMIT-NOTE
           WRITE LIMIT-RECORD END-WRITE

      *    -- IC カード + 暗証番号のみ (磁気と同額)
           MOVE SPACES TO LIMIT-RECORD
           MOVE 'I' TO LIMIT-MEDIA
           MOVE 'P' TO LIMIT-AUTH-METHOD
           MOVE 500000     TO LIMIT-PER-TXN
           MOVE 500000     TO LIMIT-DAILY-AMT
           MOVE 10         TO LIMIT-DAILY-CNT
           MOVE 'IC 暗証番号のみ' TO LIMIT-NOTE
           WRITE LIMIT-RECORD END-WRITE

      *    -- IC カード + オフライン PIN (カード内照合)
           MOVE SPACES TO LIMIT-RECORD
           MOVE 'I' TO LIMIT-MEDIA
           MOVE 'O' TO LIMIT-AUTH-METHOD
           MOVE 1000000    TO LIMIT-PER-TXN
           MOVE 2000000    TO LIMIT-DAILY-AMT
           MOVE 10         TO LIMIT-DAILY-CNT
           MOVE 'IC オフライン PIN' TO LIMIT-NOTE
           WRITE LIMIT-RECORD END-WRITE

      *    -- IC カード + 生体認証
           MOVE SPACES TO LIMIT-RECORD
           MOVE 'I' TO LIMIT-MEDIA
           MOVE 'B' TO LIMIT-AUTH-METHOD
           MOVE 1000000    TO LIMIT-PER-TXN
           MOVE 5000000    TO LIMIT-DAILY-AMT
           MOVE 20         TO LIMIT-DAILY-CNT
           MOVE 'IC 生体認証' TO LIMIT-NOTE
           WRITE LIMIT-RECORD END-WRITE.
       SL-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 祝日マスタ。昇順であること (ATMCAL が二分探索で引くため)。
      * 2026 年の国民の祝日と振替休日。
      *----------------------------------------------------------------
       SEED-HOLIDAYS SECTION.
       SH-START.
           MOVE SPACES TO HOL-RECORD
           PERFORM WRITE-HOLIDAY-LIST.
       SH-EXIT.
           EXIT.

       WRITE-HOLIDAY-LIST SECTION.
       WHL-START.
           MOVE 20260101 TO HOL-DATE
           WRITE HOL-RECORD END-WRITE
           MOVE 20260112 TO HOL-DATE
           WRITE HOL-RECORD END-WRITE
           MOVE 20260211 TO HOL-DATE
           WRITE HOL-RECORD END-WRITE
           MOVE 20260223 TO HOL-DATE
           WRITE HOL-RECORD END-WRITE
           MOVE 20260320 TO HOL-DATE
           WRITE HOL-RECORD END-WRITE
           MOVE 20260429 TO HOL-DATE
           WRITE HOL-RECORD END-WRITE
           MOVE 20260503 TO HOL-DATE
           WRITE HOL-RECORD END-WRITE
           MOVE 20260504 TO HOL-DATE
           WRITE HOL-RECORD END-WRITE
           MOVE 20260505 TO HOL-DATE
           WRITE HOL-RECORD END-WRITE
           MOVE 20260506 TO HOL-DATE
           WRITE HOL-RECORD END-WRITE
           MOVE 20260720 TO HOL-DATE
           WRITE HOL-RECORD END-WRITE
           MOVE 20260811 TO HOL-DATE
           WRITE HOL-RECORD END-WRITE
           MOVE 20260921 TO HOL-DATE
           WRITE HOL-RECORD END-WRITE
           MOVE 20260922 TO HOL-DATE
           WRITE HOL-RECORD END-WRITE
           MOVE 20260923 TO HOL-DATE
           WRITE HOL-RECORD END-WRITE
           MOVE 20261012 TO HOL-DATE
           WRITE HOL-RECORD END-WRITE
           MOVE 20261103 TO HOL-DATE
           WRITE HOL-RECORD END-WRITE
           MOVE 20261123 TO HOL-DATE
           WRITE HOL-RECORD END-WRITE.
       WHL-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 営業時間。既定は 24 時間稼働なので全区分を 00:00-24:00 とする。
      * 規制を入れる場合はこの値を変える。行を消すと「規制なし」に
      * なるので、終日休止にしたい場合は 0000-0000 を入れる。
      *----------------------------------------------------------------
       SEED-HOURS SECTION.
       SH-START.
           MOVE SPACES TO HOUR-RECORD
           MOVE 0000 TO HOUR-FROM-HHMM
           MOVE 2400 TO HOUR-TO-HHMM
           SET HOUR-DT-WEEKDAY  TO TRUE
           WRITE HOUR-RECORD END-WRITE
           SET HOUR-DT-SATURDAY TO TRUE
           WRITE HOUR-RECORD END-WRITE
           SET HOUR-DT-HOLIDAY  TO TRUE
           WRITE HOUR-RECORD END-WRITE.
       SH-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 締め状態。前営業日まで締め済み、実行中でない状態から始める。
      *----------------------------------------------------------------
       SEED-CLOSE-STATE SECTION.
       SCS-START.
           MOVE SPACES TO CLOSE-RECORD
           MOVE CN-ATM-ID TO CLS-ATM-ID
           MOVE 20260916  TO CLS-LAST-CLOSED-DATE
           MOVE 20260916180000 TO CLS-LAST-CLOSED-TS
           SET  CLS-ST-IDLE TO TRUE
           MOVE ZERO TO CLS-LAST-DIFF-CNT
           MOVE ZERO TO CLS-LAST-PENDING-CNT
           MOVE ZERO TO CLS-LAST-SEQ
           MOVE ZERO TO CLS-LAST-JRNL-SEQ
           MOVE ZERO TO CLS-LAST-ARCHIVED-DATE
           WRITE CLOSE-RECORD END-WRITE.
       SCS-EXIT.
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
