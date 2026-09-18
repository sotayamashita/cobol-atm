      *****************************************************************
      * CASHIF.cpy - ATMCASH 呼出インタフェース
      *   CALL 'ATMCASH' USING CASH-PARM ATM-SESSION
      *   PLAN で得た CASH-OUT-PLAN をそのまま DISPENSE へ引き継ぐこと。
      *   呼出元が PLAN 結果を書き換えてはならない。
      *****************************************************************
       01  CASH-PARM.
           05  CASH-FUNCTION           PIC X(08).
               88  CASH-FN-OPEN                VALUE 'OPEN    '.
               88  CASH-FN-PLAN                VALUE 'PLAN    '.
               88  CASH-FN-DISPENSE            VALUE 'DISPENSE'.
               88  CASH-FN-ACCEPT              VALUE 'ACCEPT  '.
               88  CASH-FN-CLOSE               VALUE 'CLOSE   '.
           05  CASH-IN-AMOUNT          PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  CASH-OUT-RETCODE        PIC S9(04) COMP.
           05  CASH-OUT-ERROR-CODE     PIC X(04).
           05  CASH-OUT-PLAN.
               10  CASH-PL-DENOM OCCURS 4 TIMES PIC 9(06).
               10  CASH-PL-CNT   OCCURS 4 TIMES PIC 9(03).
