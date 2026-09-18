      *****************************************************************
      * RETCODE.cpy - 共通リターンコード / 業務エラーコード定義
      *   RC-*  : モジュール間の技術的リターンコード
      *   EC-*  : 業務エラーコード (ジャーナル・画面に出力)
      *****************************************************************
       01  WS-RETCODE-CONST.
           05  RC-OK                   PIC S9(4) COMP VALUE ZERO.
           05  RC-NOTFOUND             PIC S9(4) COMP VALUE 4.
           05  RC-BUSINESS-ERROR       PIC S9(4) COMP VALUE 8.
           05  RC-IO-ERROR             PIC S9(4) COMP VALUE 12.
           05  RC-FATAL                PIC S9(4) COMP VALUE 16.

       01  WS-ERRCODE-CONST.
      *    -- 認証系 (1xxx)
           05  EC-NONE                 PIC X(04) VALUE '0000'.
           05  EC-CARD-UNKNOWN         PIC X(04) VALUE '1001'.
           05  EC-CARD-EXPIRED         PIC X(04) VALUE '1002'.
           05  EC-CARD-CAPTURED        PIC X(04) VALUE '1003'.
           05  EC-CARD-LOCKED          PIC X(04) VALUE '1004'.
           05  EC-PIN-INVALID          PIC X(04) VALUE '1005'.
      *    -- 口座系 (2xxx)
           05  EC-ACCT-UNKNOWN         PIC X(04) VALUE '2001'.
           05  EC-ACCT-FROZEN          PIC X(04) VALUE '2002'.
           05  EC-ACCT-CLOSED          PIC X(04) VALUE '2003'.
           05  EC-ACCT-DORMANT         PIC X(04) VALUE '2004'.
      *    -- 取引系 (3xxx)
           05  EC-INSUFFICIENT-FUNDS   PIC X(04) VALUE '3001'.
           05  EC-LIMIT-PER-TXN        PIC X(04) VALUE '3002'.
           05  EC-LIMIT-DAILY-AMT      PIC X(04) VALUE '3003'.
           05  EC-LIMIT-DAILY-CNT      PIC X(04) VALUE '3004'.
           05  EC-AMOUNT-INVALID       PIC X(04) VALUE '3005'.
           05  EC-AMOUNT-NOT-UNIT      PIC X(04) VALUE '3006'.
           05  EC-OUT-OF-SERVICE-HOUR  PIC X(04) VALUE '3007'.
      *    -- 現金機構系 (4xxx)
           05  EC-CASH-SHORTAGE        PIC X(04) VALUE '4001'.
           05  EC-CASH-NO-COMBINATION  PIC X(04) VALUE '4002'.
      *    -- システム系 (9xxx)
           05  EC-SYSTEM-IO            PIC X(04) VALUE '9001'.
           05  EC-SYSTEM-BUSY          PIC X(04) VALUE '9002'.
