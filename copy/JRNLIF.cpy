      *****************************************************************
      * JRNLIF.cpy - ATMJRNL 呼出インタフェース
      *   CALL 'ATMJRNL' USING JRNL-PARM ATM-SESSION
      *   レコード内容はセッションから組み立てられるため、呼出元は
      *   フェーズと結果区分だけを指定する。
      *****************************************************************
       01  JRNL-PARM.
           05  JRNL-FUNCTION           PIC X(08).
               88  JRNL-FN-OPEN                VALUE 'OPEN    '.
               88  JRNL-FN-WRITE               VALUE 'WRITE   '.
               88  JRNL-FN-CLOSE               VALUE 'CLOSE   '.
           05  JRNL-IN-PHASE           PIC X(01).
               88  JRNL-PH-START               VALUE 'S'.
               88  JRNL-PH-END                 VALUE 'E'.
               88  JRNL-PH-REVERSAL            VALUE 'R'.
           05  JRNL-IN-RESULT          PIC X(01).
               88  JRNL-RS-SUCCESS             VALUE 'S'.
               88  JRNL-RS-FAILED              VALUE 'F'.
           05  JRNL-OUT-RETCODE        PIC S9(04) COMP.
