      *****************************************************************
      * RECONREC.cpy - 突合結果 1 件
      *   締めバッチが検出した「人が見るべき事象」を表す。
      *   レポート出力と締め状態の件数集計の両方がこの形を共有する。
      *
      *   種別ごとに埋まる項目が違うので、金額・件数はすべて汎用の
      *   数値項目で持ち、意味は種別で読み替える。帳票 1 行に載る
      *   情報だけを持たせ、それ以上は EJ を引いてもらう方針。
      *****************************************************************
       01  RECON-RECORD.
           05  RCN-TYPE                PIC X(02).
      *        -- 不確定取引 (S はあるが E が無い)。電源断・異常終了。
               88  RCN-TP-PENDING              VALUE 'PN'.
      *        -- 成否不明の他行為替。追跡番号で相手行に照会する。
               88  RCN-TP-ZENGIN-UNKNOWN       VALUE 'ZU'.
      *        -- 現金の理論値と実査値の差異。
               88  RCN-TP-CASH-DIFF            VALUE 'CD'.
      *        -- 取消が失敗したまま残った取引。
               88  RCN-TP-REVERSAL-FAILED      VALUE 'RF'.
      *        -- 実査で数えられなかったカセット。実査を試みたのに
      *        -- 一部が漏れた場合だけ出す。どれが漏れたかを帳票に
      *        -- 残さないと、翌朝に帳票を読む係員が再実査の範囲を
      *        -- 決められない。
               88  RCN-TP-NOT-COUNTED          VALUE 'NC'.
           05  RCN-TXN-ID              PIC X(12).
           05  RCN-SESSION-ID          PIC X(12).
           05  RCN-TIMESTAMP           PIC 9(14).
           05  RCN-TXN-TYPE            PIC X(02).
           05  RCN-ACCT-NO             PIC X(10).
           05  RCN-TRACE-NO            PIC X(12).
      *    -- 金額または金種の額面。種別で読み替える
           05  RCN-AMOUNT              PIC S9(13)V99 SIGN LEADING SEPARATE.
      *    -- 理論値と実査値。現金差異のときだけ使う
           05  RCN-EXPECTED            PIC S9(09) SIGN LEADING SEPARATE.
           05  RCN-ACTUAL              PIC S9(09) SIGN LEADING SEPARATE.
           05  RCN-ERROR-CODE          PIC X(04).
      *    -- レコード長は 128 バイト。RPTIF の RPT-IN-RECON と同じ長さ
      *    -- にしておかないと、帳票へ渡す途中で末尾が欠ける。
           05  FILLER                  PIC X(24).
