      *****************************************************************
      * FEEREC.cpy - ATM 利用手数料マスタ (順編成 / 起動時に全件読込)
      *   レコード長 32 バイト固定
      *
      *   実際の銀行の手数料は「曜日区分 × 時間帯 × カード区分」の
      *   三次元で決まる。時刻だけでは決まらない。
      *     例) 平日 8:45-18:00 無料 / 平日それ以外 110 円
      *         土曜 9:00-14:00 110 円 / 日曜・祝日 220 円
      *   祝日は日曜と同じ区分 (H) に寄せる。判定は ATMCAL が行う。
      *
      *   FEE-FROM-HHMM <= 時刻 < FEE-TO-HHMM で突き合わせる。
      *   同一区分で複数行が該当した場合は、先に読んだ行を採用する。
      *****************************************************************
       01  FEE-RECORD.
           05  FEE-CARD-KIND           PIC X(01).
               88  FEE-CK-OWN                  VALUE 'O'.
               88  FEE-CK-PARTNER              VALUE 'P'.
           05  FEE-DAY-TYPE            PIC X(01).
               88  FEE-DT-WEEKDAY              VALUE 'W'.
               88  FEE-DT-SATURDAY             VALUE 'S'.
               88  FEE-DT-HOLIDAY              VALUE 'H'.
           05  FEE-TXN-TYPE            PIC X(02).
           05  FEE-FROM-HHMM           PIC 9(04).
           05  FEE-TO-HHMM             PIC 9(04).
           05  FEE-AMOUNT              PIC 9(09).
           05  FILLER                  PIC X(11).
