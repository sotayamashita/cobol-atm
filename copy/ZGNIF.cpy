      *****************************************************************
      * ZGNIF.cpy - ATMZGN (全銀システム接続) 呼出インタフェース
      *   CALL 'ATMZGN' USING ZGN-PARM ATM-SESSION
      *
      *   ROUTE : 相手行と現在時刻から経路と入金日を決める (通信しない)
      *   SEND  : 為替電文を送る。応答なしはタイムアウトとして扱う
      *
      *   タイムアウトは「失敗」ではなく「成否不明」である点が重要。
      *   二重送信を避けるため、呼出元は必ず取消電文を送るか、
      *   不確定取引として EJ に残して日次で補正する。
      *****************************************************************
       01  ZGN-PARM.
           05  ZGN-FUNCTION            PIC X(08).
               88  ZGN-FN-ROUTE                VALUE 'ROUTE   '.
               88  ZGN-FN-SEND                 VALUE 'SEND    '.
               88  ZGN-FN-CANCEL               VALUE 'CANCEL  '.
               88  ZGN-FN-CLOSE                VALUE 'CLOSE   '.
           05  ZGN-IN-BANK-CD          PIC X(04).
           05  ZGN-IN-ACCT-NO          PIC X(10).
           05  ZGN-IN-AMOUNT           PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  ZGN-OUT-RETCODE         PIC S9(04) COMP.
           05  ZGN-OUT-ERROR-CODE      PIC X(04).
           05  ZGN-OUT-BANK-NAME       PIC X(30).
      *    -- 決まった経路
      *       C = コアタイム (平日 8:30-15:30)
      *       M = モアタイム (夜間・休日、相手行が参加している場合)
      *       N = 翌営業日扱い (相手行が未参加・接続時間外)
           05  ZGN-OUT-ROUTE           PIC X(01).
               88  ZGN-RT-CORE                 VALUE 'C'.
               88  ZGN-RT-MORETIME             VALUE 'M'.
               88  ZGN-RT-NEXT-DAY             VALUE 'N'.
      *    -- 実際に資金が動く日
           05  ZGN-OUT-VALUE-DATE      PIC 9(08).
      *    -- 即時着金したか (N のときは 'N')
           05  ZGN-OUT-IMMEDIATE       PIC X(01).
      *    -- 送信した電文の追跡番号。取消電文で同じ番号を使う
           05  ZGN-OUT-TRACE-NO        PIC X(12).
