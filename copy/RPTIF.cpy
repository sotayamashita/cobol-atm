      *****************************************************************
      * RPTIF.cpy - ATMRPT (締めレポート出力) 呼出インタフェース
      *   CALL 'ATMRPT' USING RPT-PARM ATM-SESSION
      *
      *   帳票は人が読むもので、突合の判定そのものではない。整形を
      *   このモジュールに閉じ込め、締めバッチ側は「何を検出したか」
      *   だけを渡す。出力先を印刷装置やホストへ変える場合も、
      *   差し替えはここだけで済む。
      *
      *   呼出順は OPEN → HEADER → (SUMMARY | DETAIL)* → FOOTER → CLOSE。
      *****************************************************************
       01  RPT-PARM.
           05  RPT-FUNCTION            PIC X(08).
               88  RPT-FN-OPEN                 VALUE 'OPEN    '.
               88  RPT-FN-HEADER               VALUE 'HEADER  '.
      *        -- 集計行 (取引種別ごとの件数と金額)
               88  RPT-FN-SUMMARY              VALUE 'SUMMARY '.
      *        -- 明細行 (突合で検出した 1 件)
               88  RPT-FN-DETAIL               VALUE 'DETAIL  '.
               88  RPT-FN-FOOTER               VALUE 'FOOTER  '.
               88  RPT-FN-CLOSE                VALUE 'CLOSE   '.
           05  RPT-IN-BUSINESS-DATE    PIC 9(08).
      *    -- SUMMARY 用
           05  RPT-IN-LABEL            PIC X(24).
           05  RPT-IN-COUNT            PIC 9(07).
           05  RPT-IN-AMOUNT           PIC S9(13)V99 SIGN LEADING SEPARATE.
      *    -- DETAIL 用。RECONREC.cpy と同じ並びを渡す
           05  RPT-IN-RECON            PIC X(128).
      *    -- FOOTER 用
           05  RPT-IN-DIFF-CNT         PIC 9(05).
           05  RPT-IN-PENDING-CNT      PIC 9(05).
           05  RPT-OUT-RETCODE         PIC S9(04) COMP.
