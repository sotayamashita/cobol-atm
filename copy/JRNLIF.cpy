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
      *        -- 締めバッチ用の走査。EJ の FD を持つのはこのモジュール
      *        -- だけなので、読み出しもここを通す。書込用のオープンと
      *        -- は別系統で、走査中に追記は行わない。
               88  JRNL-FN-SCAN-OPEN           VALUE 'SCANOPEN'.
               88  JRNL-FN-SCAN-NEXT           VALUE 'SCANNEXT'.
               88  JRNL-FN-SCAN-CLOSE          VALUE 'SCANCLOS'.
      *        -- 取引通番の採番。EJ の通番は再起動をまたいで単調増加
      *        -- するため、端末内で一意な番号の出どころとして使える。
      *        -- 時刻とセッション内連番で作ると、プロセスをまたいで
      *        -- 衝突する (連番がセッションごとに 1 へ戻るため)。
               88  JRNL-FN-NEXT-TXN            VALUE 'NEXTTXN '.
           05  JRNL-IN-PHASE           PIC X(01).
               88  JRNL-PH-START               VALUE 'S'.
               88  JRNL-PH-END                 VALUE 'E'.
               88  JRNL-PH-REVERSAL            VALUE 'R'.
           05  JRNL-IN-RESULT          PIC X(01).
               88  JRNL-RS-SUCCESS             VALUE 'S'.
               88  JRNL-RS-FAILED              VALUE 'F'.
           05  JRNL-OUT-RETCODE        PIC S9(04) COMP.
      *    -- 走査で読んだ 1 件。SCANNEXT が終端に達したら 'Y'
           05  JRNL-OUT-EOF            PIC X(01).
           05  JRNL-OUT-RECORD         PIC X(200).
           05  JRNL-OUT-TXN-NO         PIC 9(09).
