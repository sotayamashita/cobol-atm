      *****************************************************************
      * ACCTIF.cpy - ATMACCT 呼出インタフェース
      *   CALL 'ATMACCT' USING ACCT-PARM ATM-SESSION
      *   READLOCK → UPDATE は同一モジュール呼出の中で完結させること。
      *   間に他口座の I/O を挟むとロックが解放される。
      *****************************************************************
       01  ACCT-PARM.
           05  ACCT-FUNCTION           PIC X(08).
               88  ACCT-FN-READ                VALUE 'READ    '.
               88  ACCT-FN-LOCK                VALUE 'READLOCK'.
               88  ACCT-FN-UPDATE              VALUE 'UPDATE  '.
      *        -- UNLOCK は取引単位、CLOSE は端末単位。取引の異常終了で
      *        -- ファイルごと閉じると次取引で索引の再オープンが要る。
               88  ACCT-FN-UNLOCK              VALUE 'UNLOCK  '.
               88  ACCT-FN-CLOSE               VALUE 'CLOSE   '.
           05  ACCT-IN-ACCT-NO         PIC X(10).
           05  ACCT-OUT-RETCODE        PIC S9(04) COMP.
           05  ACCT-OUT-ERROR-CODE     PIC X(04).
           05  ACCT-IO-RECORD          PIC X(160).
