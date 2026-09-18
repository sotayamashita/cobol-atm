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
      *        -- 当日分の EJ を日付つきのファイルへ退避し、現用の EJ を
      *        -- 空にする。追記専用のまま伸ばし続けると、起動時の通番
      *        -- 復元と締めの走査が運用日数に比例して重くなるため。
      *        -- 退避済みファイルは消さない。保存年限は監査要件であり、
      *        -- 削除は運用側が決めること。
               88  JRNL-FN-ARCHIVE             VALUE 'ARCHIVE '.
      *    -- ARCHIVE の退避先を決める営業日
           05  JRNL-IN-ARCHIVE-DATE    PIC 9(08).
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
           05  JRNL-OUT-ARCHIVED-CNT   PIC 9(09).
