      *****************************************************************
      * PROGRAM : ATMRPT
      * PURPOSE : 日次締めレポート (data/atmrpt.txt) 出力モジュール
      * DESIGN  :
      *   - 整形専用。「差異があるか」「係員対応が要るか」の判定は
      *     一切しない。件数も突合結果も呼出元が決めた値をそのまま
      *     受け取り、人が読める形に直して書くだけにする。判定を
      *     ここに持たせると、帳票の見た目を変えるたびに締めの判断
      *     基準を触ることになるため。
      *   - 1 行は WS-LINE に STRING で組み立てる。日本語見出しを
      *     固定長項目の VALUE に置くと UTF-8 で 1 文字 3 バイトに
      *     なり桁が合わないため、リテラルは STRING の被連結側に
      *     だけ置き、桁揃えは ASCII 項目の側で取る。
      *   - 表示の語彙はこのモジュールが持つ。取引種別コードを受け
      *     取って和文へ直すのも整形の一部であり、呼出元に和文を
      *     持たせると帳票の言い回しが業務ロジック側へ漏れる。
      *   - 金額は編集項目に MOVE してから連結する。係員が目視で
      *     照合するので、桁区切りが無いと読めない。
      *   - DETAIL は RCN-TYPE ごとに出す項目を変える。全項目を
      *     並べると空欄だらけになり、かえって異常が埋もれる。
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMRPT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN USING WS-RPT-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE.
       01  RPT-RECORD                  PIC X(256).

       WORKING-STORAGE SECTION.
       01  WS-RPT-STATUS               PIC X(02) VALUE '00'.
      *    -- 帳票は端末ごと。ファイル名に端末 ID を含める。
       01  WS-RPT-NAME                 PIC X(64) VALUE SPACES.
       01  WS-OPENED                   PIC X(01) VALUE 'N'.
       01  WS-LINE                     PIC X(256) VALUE SPACES.
       01  WS-RULE                     PIC X(96)  VALUE ALL '-'.
       01  WS-LABEL                    PIC X(24)  VALUE SPACES.

      *    -- 現在日時。出力時刻は帳票の同一性判断に使うため必ず入れる
       01  WS-NOW                      PIC X(21) VALUE SPACES.
       01  WS-NOW-R REDEFINES WS-NOW.
           05  WS-NOW-YYYY             PIC 9(04).
           05  WS-NOW-MM               PIC 9(02).
           05  WS-NOW-DD               PIC 9(02).
           05  WS-NOW-HH               PIC 9(02).
           05  WS-NOW-MI               PIC 9(02).
           05  WS-NOW-SS               PIC 9(02).
           05  FILLER                  PIC X(07).

      *    -- 編集用作業領域
       01  WS-EDIT-AREA.
           05  WS-ED-DATE              PIC X(10) VALUE SPACES.
           05  WS-ED-DATETIME          PIC X(19) VALUE SPACES.
           05  WS-ED-AMOUNT            PIC -,---,---,---,--9.99.
           05  WS-ED-EXPECTED          PIC -,---,---,--9.
           05  WS-ED-ACTUAL            PIC -,---,---,--9.
           05  WS-ED-DIFF              PIC -,---,---,--9.
           05  WS-ED-COUNT             PIC ---,---,--9.
           05  WS-ED-LABEL             PIC X(24) VALUE SPACES.

      *    -- 突合結果 1 件の展開先。X(128) で受け取ったものを
      *    -- 名前付きで読むために WORKING-STORAGE 側に置く
       COPY 'RECONREC.cpy'.
       COPY 'RETCODE.cpy'.

       LINKAGE SECTION.
       COPY 'RPTIF.cpy'.
       COPY 'ATMSESS.cpy'.

       PROCEDURE DIVISION USING RPT-PARM ATM-SESSION.

       MAIN-CONTROL SECTION.
       MAIN-START.
           MOVE RC-OK TO RPT-OUT-RETCODE
           EVALUATE TRUE
               WHEN RPT-FN-OPEN     PERFORM OPEN-REPORT
               WHEN RPT-FN-HEADER   PERFORM WRITE-HEADER
               WHEN RPT-FN-SUMMARY  PERFORM WRITE-SUMMARY
               WHEN RPT-FN-DETAIL   PERFORM WRITE-DETAIL
               WHEN RPT-FN-FOOTER   PERFORM WRITE-FOOTER
               WHEN RPT-FN-CLOSE    PERFORM CLOSE-REPORT
               WHEN OTHER           MOVE RC-FATAL TO RPT-OUT-RETCODE
           END-EVALUATE
           GOBACK.

      *----------------------------------------------------------------
      * OPEN : 常に OUTPUT で開き直す
      *   帳票は営業日ごとに 1 通で、追記すると前日分と混ざって
      *   どこからが当日分か読めなくなるため、既存を置き換える。
      *----------------------------------------------------------------
       OPEN-REPORT SECTION.
       OPEN-R-START.
           IF WS-OPENED = 'Y'
               GO TO OPEN-R-EXIT
           END-IF

           MOVE SPACES TO WS-RPT-NAME
           STRING 'data/atmrpt-' DELIMITED BY SIZE
                  SESS-ATM-ID    DELIMITED BY SIZE
                  '.txt'         DELIMITED BY SIZE
               INTO WS-RPT-NAME
           END-STRING

           OPEN OUTPUT RPT-FILE
           IF WS-RPT-STATUS NOT = '00' AND WS-RPT-STATUS NOT = '05'
               MOVE RC-IO-ERROR TO RPT-OUT-RETCODE
           ELSE
               MOVE 'Y' TO WS-OPENED
           END-IF.
       OPEN-R-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * HEADER : 表題・端末・営業日・出力日時
      *----------------------------------------------------------------
       WRITE-HEADER SECTION.
       WRITE-H-START.
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           PERFORM EDIT-BUSINESS-DATE

           PERFORM WRITE-RULE

           MOVE SPACES TO WS-LINE
           STRING '  ATM 日次締めレポート'
               DELIMITED BY SIZE INTO WS-LINE
           PERFORM WRITE-LINE

           MOVE SPACES TO WS-LINE
           STRING '  端末 ID : ' DELIMITED BY SIZE
                  SESS-ATM-ID    DELIMITED BY SIZE
               INTO WS-LINE
           PERFORM WRITE-LINE

           MOVE SPACES TO WS-LINE
           STRING '  営業日  : ' DELIMITED BY SIZE
                  WS-ED-DATE     DELIMITED BY SIZE
               INTO WS-LINE
           PERFORM WRITE-LINE

           STRING WS-NOW-YYYY '-' WS-NOW-MM '-' WS-NOW-DD ' '
                  WS-NOW-HH ':' WS-NOW-MI ':' WS-NOW-SS
               DELIMITED BY SIZE INTO WS-ED-DATETIME
           MOVE SPACES TO WS-LINE
           STRING '  出力日時: ' DELIMITED BY SIZE
                  WS-ED-DATETIME DELIMITED BY SIZE
               INTO WS-LINE
           PERFORM WRITE-LINE

           PERFORM WRITE-RULE.
       WRITE-H-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * SUMMARY : ラベル + 件数 + 金額を 1 行
      *   ラベルは X(24) 固定なのでそのまま連結すれば列が揃う。
      *----------------------------------------------------------------
       WRITE-SUMMARY SECTION.
       WRITE-S-START.
           PERFORM RESOLVE-LABEL
           MOVE WS-LABEL      TO WS-ED-LABEL
           MOVE RPT-IN-COUNT  TO WS-ED-COUNT
           MOVE RPT-IN-AMOUNT TO WS-ED-AMOUNT

           MOVE SPACES TO WS-LINE
           STRING '  '            DELIMITED BY SIZE
                  WS-ED-LABEL     DELIMITED BY SIZE
                  ' '             DELIMITED BY SIZE
                  WS-ED-COUNT     DELIMITED BY SIZE
                  ' 件  '         DELIMITED BY SIZE
                  WS-ED-AMOUNT    DELIMITED BY SIZE
                  ' 円'           DELIMITED BY SIZE
               INTO WS-LINE
           PERFORM WRITE-LINE.
       WRITE-S-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * DETAIL : 突合結果 1 件を種別に応じた 1 行にする
      *   種別ごとに係員が次に取る行動が違う。PN は EJ を引く、
      *   ZU は相手行に追跡番号で照会する、CD は現金を数え直す、
      *   RF は取消の再投入。行動に要る項目だけを載せる。
      *----------------------------------------------------------------
       WRITE-DETAIL SECTION.
       WRITE-D-START.
           MOVE SPACES         TO RECON-RECORD
           MOVE RPT-IN-RECON   TO RECON-RECORD
           MOVE RCN-AMOUNT     TO WS-ED-AMOUNT
           MOVE SPACES         TO WS-LINE

           EVALUATE TRUE
               WHEN RCN-TP-PENDING          PERFORM DETAIL-PENDING
               WHEN RCN-TP-ZENGIN-UNKNOWN   PERFORM DETAIL-ZENGIN
               WHEN RCN-TP-CASH-DIFF        PERFORM DETAIL-CASH-DIFF
               WHEN RCN-TP-REVERSAL-FAILED  PERFORM DETAIL-REVERSAL
               WHEN RCN-TP-NOT-COUNTED      PERFORM DETAIL-NOT-COUNTED
               WHEN OTHER                   PERFORM DETAIL-UNKNOWN
           END-EVALUATE

           PERFORM WRITE-LINE.
       WRITE-D-EXIT.
           EXIT.

      *    -- 不確定取引: EJ を引くのに取引 ID と発生時刻が要る
       DETAIL-PENDING SECTION.
       DP-START.
           PERFORM EDIT-RCN-TIMESTAMP
           STRING '  [PN] 不確定取引  取引=' DELIMITED BY SIZE
                  RCN-TXN-ID              DELIMITED BY SIZE
                  ' 種別=' DELIMITED BY SIZE
                  RCN-TXN-TYPE            DELIMITED BY SIZE
                  ' 口座=' DELIMITED BY SIZE
                  RCN-ACCT-NO             DELIMITED BY SIZE
                  ' 金額=' DELIMITED BY SIZE
                  WS-ED-AMOUNT            DELIMITED BY SIZE
                  ' 発生=' DELIMITED BY SIZE
                  WS-ED-DATETIME          DELIMITED BY SIZE
               INTO WS-LINE.
       DP-EXIT.
           EXIT.

      *    -- 成否不明の他行為替: 相手行への照会キーは追跡番号
       DETAIL-ZENGIN SECTION.
       DZ-START.
           PERFORM EDIT-RCN-TIMESTAMP
           STRING '  [ZU] 他行為替成否不明  追跡番号=' DELIMITED BY SIZE
                  RCN-TRACE-NO            DELIMITED BY SIZE
                  ' 取引=' DELIMITED BY SIZE
                  RCN-TXN-ID              DELIMITED BY SIZE
                  ' 金額=' DELIMITED BY SIZE
                  WS-ED-AMOUNT            DELIMITED BY SIZE
                  ' エラー=' DELIMITED BY SIZE
                  RCN-ERROR-CODE          DELIMITED BY SIZE
                  ' 発生=' DELIMITED BY SIZE
                  WS-ED-DATETIME          DELIMITED BY SIZE
               INTO WS-LINE.
       DZ-EXIT.
           EXIT.

      *    -- 現金差異: 理論値・実査値と、その差を明示する。
      *       差の符号は係員が読み違えやすいので引き算をこちらで
      *       済ませる。これは突合の判定ではなく表示の都合。
       DETAIL-CASH-DIFF SECTION.
       DC-START.
           MOVE RCN-EXPECTED TO WS-ED-EXPECTED
           MOVE RCN-ACTUAL   TO WS-ED-ACTUAL
           COMPUTE WS-ED-DIFF = RCN-ACTUAL - RCN-EXPECTED
           STRING '  [CD] 現金差異  理論=' DELIMITED BY SIZE
                  WS-ED-EXPECTED          DELIMITED BY SIZE
                  ' 実査=' DELIMITED BY SIZE
                  WS-ED-ACTUAL            DELIMITED BY SIZE
                  ' 差=' DELIMITED BY SIZE
                  WS-ED-DIFF              DELIMITED BY SIZE
                  ' 券種額面=' DELIMITED BY SIZE
                  WS-ED-AMOUNT            DELIMITED BY SIZE
               INTO WS-LINE.
       DC-EXIT.
           EXIT.

      *    -- 取消失敗: 再投入の対象を特定する ID 類とエラー要因
       DETAIL-REVERSAL SECTION.
       DR-START.
           PERFORM EDIT-RCN-TIMESTAMP
           STRING '  [RF] 取消失敗  取引=' DELIMITED BY SIZE
                  RCN-TXN-ID              DELIMITED BY SIZE
                  ' セッション=' DELIMITED BY SIZE
                  RCN-SESSION-ID          DELIMITED BY SIZE
                  ' 口座=' DELIMITED BY SIZE
                  RCN-ACCT-NO             DELIMITED BY SIZE
                  ' 金額=' DELIMITED BY SIZE
                  WS-ED-AMOUNT            DELIMITED BY SIZE
                  ' エラー=' DELIMITED BY SIZE
                  RCN-ERROR-CODE          DELIMITED BY SIZE
                  ' 発生=' DELIMITED BY SIZE
                  WS-ED-DATETIME          DELIMITED BY SIZE
               INTO WS-LINE.
       DR-EXIT.
           EXIT.

      *    -- 未計数: 差異ではなく「差異が判らない」。理論値だけ出し、
      *    -- 実査欄は書かない。ゼロ枚と読めてしまうため。
       DETAIL-NOT-COUNTED SECTION.
       DNC-START.
           MOVE RCN-EXPECTED TO WS-ED-EXPECTED
           STRING '  [NC] 実査未計数  理論=' DELIMITED BY SIZE
                  WS-ED-EXPECTED            DELIMITED BY SIZE
                  ' 実査=(未計数) 券種額面=' DELIMITED BY SIZE
                  WS-ED-AMOUNT              DELIMITED BY SIZE
               INTO WS-LINE.
       DNC-EXIT.
           EXIT.

      *    -- 未知の種別。ここで捨てると係員が異常に気付けないので、
      *    -- 読める形にならなくても種別と取引 ID だけは必ず出す。
       DETAIL-UNKNOWN SECTION.
       DU-START.
           STRING '  [' DELIMITED BY SIZE
                  RCN-TYPE                DELIMITED BY SIZE
                  '] 未定義の突合種別  取引=' DELIMITED BY SIZE
                  RCN-TXN-ID              DELIMITED BY SIZE
                  ' 金額=' DELIMITED BY SIZE
                  WS-ED-AMOUNT            DELIMITED BY SIZE
               INTO WS-LINE.
       DU-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * FOOTER : 件数の合計と、呼出元が決めた要対応の別
      *   「何をもって要対応とするか」は締めの判断基準なので、ここで
      *   件数から導かない。導くと、基準を変えたいときに帳票モジュール
      *   を触ることになる。
      *----------------------------------------------------------------
       WRITE-FOOTER SECTION.
       WRITE-F-START.
           PERFORM WRITE-RULE

           MOVE RPT-IN-DIFF-CNT TO WS-ED-COUNT
           MOVE SPACES TO WS-LINE
           STRING '  差異件数    : ' DELIMITED BY SIZE
                  WS-ED-COUNT         DELIMITED BY SIZE
                  ' 件' DELIMITED BY SIZE
               INTO WS-LINE
           PERFORM WRITE-LINE

           MOVE RPT-IN-PENDING-CNT TO WS-ED-COUNT
           MOVE SPACES TO WS-LINE
           STRING '  不確定件数  : ' DELIMITED BY SIZE
                  WS-ED-COUNT         DELIMITED BY SIZE
                  ' 件' DELIMITED BY SIZE
               INTO WS-LINE
           PERFORM WRITE-LINE

      *    -- 実査の有無は差異件数とは別の事実。未実施を黙って
      *    -- 「差異なし」と並べると、実施して問題なしと読めてしまう。
           MOVE SPACES TO WS-LINE
           IF RPT-CASH-COUNTED
               STRING '  現金実査    : 実施済' DELIMITED BY SIZE
                   INTO WS-LINE
           ELSE
               STRING '  現金実査    : 未実施 '
                      '(数えていないカセットは NC で示す)'
                   DELIMITED BY SIZE INTO WS-LINE
           END-IF
           PERFORM WRITE-LINE

           IF RPT-TRUNCATED
               MOVE SPACES TO WS-LINE
               STRING '  ** 検出件数が上限に達し、明細を打ち切りました。'
                      '記載漏れがあります。**'
                   DELIMITED BY SIZE INTO WS-LINE
               PERFORM WRITE-LINE
           END-IF

           MOVE SPACES TO WS-LINE
           IF RPT-ACTION-YES
               STRING '  ** 係員対応が必要です。'
                      '上記の明細を確認してください。**'
                   DELIMITED BY SIZE INTO WS-LINE
           ELSE
               STRING '  要対応事象はありません。'
                   DELIMITED BY SIZE INTO WS-LINE
           END-IF
           PERFORM WRITE-LINE

           PERFORM WRITE-RULE.
       WRITE-F-EXIT.
           EXIT.

       CLOSE-REPORT SECTION.
       CLOSE-R-START.
           IF WS-OPENED = 'Y'
               CLOSE RPT-FILE
               MOVE 'N' TO WS-OPENED
           END-IF.
       CLOSE-R-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 共通下請け
      *----------------------------------------------------------------
      *    -- 未オープンのまま書くと帳票が欠ける。障害として返し、
      *    -- ここで勝手に開かない (OPEN は営業日の区切りを兼ねる)
      *----------------------------------------------------------------
      * ラベルの解決。取引種別コードで渡されたものは和文に直す。
      * それ以外はそのまま使う (現金の増減など、呼出元が文言を
      * 決めたほうが自然な行のため)。
      *----------------------------------------------------------------
       RESOLVE-LABEL SECTION.
       RL-START.
           EVALUATE RPT-IN-LABEL (1:2)
               WHEN 'IQ' MOVE '残高照会'     TO WS-LABEL
               WHEN 'WD' MOVE 'お引出し'     TO WS-LABEL
               WHEN 'DP' MOVE 'お預入れ'     TO WS-LABEL
               WHEN 'TR' MOVE 'お振込み'     TO WS-LABEL
               WHEN 'PC' MOVE '暗証番号変更' TO WS-LABEL
               WHEN OTHER MOVE RPT-IN-LABEL  TO WS-LABEL
           END-EVALUATE.
       RL-EXIT.
           EXIT.

       WRITE-LINE SECTION.
       WL-START.
           IF WS-OPENED NOT = 'Y'
               MOVE RC-IO-ERROR TO RPT-OUT-RETCODE
               GO TO WL-EXIT
           END-IF

           MOVE WS-LINE TO RPT-RECORD
           WRITE RPT-RECORD
           IF WS-RPT-STATUS NOT = '00'
               MOVE RC-IO-ERROR TO RPT-OUT-RETCODE
           END-IF.
       WL-EXIT.
           EXIT.

       WRITE-RULE SECTION.
       WR-START.
           MOVE SPACES  TO WS-LINE
           MOVE WS-RULE TO WS-LINE
           PERFORM WRITE-LINE.
       WR-EXIT.
           EXIT.

       EDIT-BUSINESS-DATE SECTION.
       EBD-START.
           MOVE SPACES TO WS-ED-DATE
           STRING RPT-IN-BUSINESS-DATE (1:4) '-'
                  RPT-IN-BUSINESS-DATE (5:2) '-'
                  RPT-IN-BUSINESS-DATE (7:2)
               DELIMITED BY SIZE INTO WS-ED-DATE.
       EBD-EXIT.
           EXIT.

       EDIT-RCN-TIMESTAMP SECTION.
       ERT-START.
           MOVE SPACES TO WS-ED-DATETIME
           STRING RCN-TIMESTAMP (1:4)  '-'
                  RCN-TIMESTAMP (5:2)  '-'
                  RCN-TIMESTAMP (7:2)  ' '
                  RCN-TIMESTAMP (9:2)  ':'
                  RCN-TIMESTAMP (11:2) ':'
                  RCN-TIMESTAMP (13:2)
               DELIMITED BY SIZE INTO WS-ED-DATETIME.
       ERT-EXIT.
           EXIT.

       END PROGRAM ATMRPT.
