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
      *        -- 退避済み EJ の保存年限管理。保存年限そのものは監査要件
      *        -- で運用が決めることなので、このモジュールは「いつより
      *        -- 前を対象にするか」を言われたとおりに扱うだけにする。
      *        -- 既定値を持たせると、方針を決めないまま消える。
               88  JRNL-FN-PURGE               VALUE 'PURGE   '.
      *    -- ARCHIVE の退避先を決める営業日
           05  JRNL-IN-ARCHIVE-DATE    PIC 9(08).
      *    -- PURGE の対象。この営業日より前の退避ファイルを対象とする
           05  JRNL-IN-PURGE-BEFORE    PIC 9(08).
      *    -- PURGE で遡る日数。ディレクトリを列挙する標準的な手段が
      *    -- 無いので日付を総当たりする。走査範囲は呼出元が決める。
           05  JRNL-IN-PURGE-DAYS      PIC 9(05).
      *    -- 消す前に対象を示せるよう、数えるだけの実行を用意する。
      *    -- 元に戻せない操作なので、確認の機会を挟めるようにする。
           05  JRNL-IN-PURGE-MODE      PIC X(01).
               88  JRNL-PG-LIST                VALUE 'L'.
               88  JRNL-PG-DELETE              VALUE 'D'.
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
      *    -- PURGE の対象件数と、その日付の範囲
           05  JRNL-OUT-PURGED-CNT     PIC 9(09).
           05  JRNL-OUT-PURGE-OLDEST   PIC 9(08).
           05  JRNL-OUT-PURGE-NEWEST   PIC 9(08).
