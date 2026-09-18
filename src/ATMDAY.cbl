      *****************************************************************
      * PROGRAM : ATMDAY
      * PURPOSE : 日次締めバッチ (不確定取引の抽出・現金突合・繰越)
      * DESIGN  :
      *   端末が停止している時間帯に流す。オンライン中に走らせると
      *   在庫と EJ が動き続け、突合した瞬間の値が意味を持たない。
      *
      *   処理順序は次のとおりで、順序自体に意味がある。
      *     (1) 締め状態を確認して実行中フラグを立てる
      *     (2) EJ を走査し、取引単位に突き合わせる
      *     (3) 現金の帳簿値と実査値を突き合わせる
      *     (4) 帳票を出す
      *     (5) 当日計をクリアして営業日を繰り越し、締め状態を更新する
      *   検出 (2)(3) を繰越 (5) より前に置くのは、繰越が当日計を消す
      *   ためである。先に消すと差異の原因が追えなくなる。
      *
      *   二重実行は弾く。締めは営業日の取引を確定させる操作なので、
      *   同じ日に二度流すと一度目に検出した差異が消えて追跡できない。
      *
      *   実行中フラグが残っていた場合 (前回異常終了) も弾く。自動で
      *   解除すると多重実行を許してしまうため、係員の解除を待つ。
      *
      *   [EJ の突合方法]
      *   EJ は追記専用で、1 取引につき S (開始) と E (終了) を書く。
      *   したがって「S があって E が無い取引」は途中で落ちた取引で、
      *   現金が出たかどうかが判らない。これを不確定取引として抽出する。
      *   EJ は時系列順なので、取引 ID をキーに保持して E が来たら消す、
      *   という 1 パスの処理で足りる。ソートは不要。
      *
      *   [現金の突合]
      *   本来は係員が実査した枚数を入力する。本実装では実査値の入力
      *   経路が無いため、帳簿値をそのまま実査値とみなす。実査値を
      *   受け取る口を IN-COUNT として用意してあり、係員操作パネルを
      *   作る際はそこを繋ぐ。
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMDAY.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CONST.
      *    -- 未決着の取引を保持する上限。1 営業日の取引数を超える
      *    -- ことはないが、溢れた場合は帳票で明示する。
           05  WS-MAX-OPEN-TXN         PIC S9(04) COMP VALUE 500.
           05  WS-MAX-RECON            PIC S9(04) COMP VALUE 500.
           05  WS-TXN-TYPE-CNT         PIC S9(04) COMP VALUE 6.

      *    -- EJ 走査中、まだ E が来ていない取引
       01  WS-OPEN-TXN-TABLE.
           05  WS-OPEN-CNT             PIC S9(04) COMP VALUE ZERO.
           05  WS-OPEN-ENTRY OCCURS 500 TIMES.
               10  WS-OPEN-TXN-ID      PIC X(12).
               10  WS-OPEN-RECORD      PIC X(200).

      *    -- 検出した突合結果
       01  WS-RECON-TABLE.
           05  WS-RECON-CNT            PIC S9(04) COMP VALUE ZERO.
           05  WS-RECON-ENTRY OCCURS 500 TIMES PIC X(128).

      *    -- 取引種別ごとの集計
      *    -- 取引種別の一覧は 1 箇所で持つ。初期化と集計と帳票で
      *    -- 別々に並べると、種別を増やすたびに三箇所直すことになる。
       01  WS-SUM-TYPES.
           05  FILLER PIC X(02) VALUE 'IQ'.
           05  FILLER PIC X(02) VALUE 'WD'.
           05  FILLER PIC X(02) VALUE 'DP'.
           05  FILLER PIC X(02) VALUE 'TR'.
           05  FILLER PIC X(02) VALUE 'PC'.
           05  FILLER PIC X(02) VALUE 'LD'.
       01  WS-SUM-TYPE-LIST REDEFINES WS-SUM-TYPES.
           05  WS-SUM-TYPE OCCURS 6 TIMES PIC X(02).

       01  WS-SUMMARY-TABLE.
           05  WS-SUM-ENTRY OCCURS 6 TIMES.
               10  WS-SUM-CNT          PIC 9(07).
               10  WS-SUM-AMT          PIC S9(13)V99.

       01  WS-WORK.
           05  WS-I                    PIC S9(04) COMP VALUE ZERO.
           05  WS-FOUND                PIC S9(04) COMP VALUE ZERO.
           05  WS-DIFF-CNT             PIC 9(05) VALUE ZERO.
           05  WS-PENDING-CNT          PIC 9(05) VALUE ZERO.
           05  WS-OVERFLOW             PIC X(01) VALUE 'N'.
           05  WS-ABORT                PIC X(01) VALUE 'N'.
           05  WS-ACTION               PIC X(01) VALUE 'N'.
      *    -- 実査枚数と、それが入力されたか。入力経路が無い間は
      *    -- 'N' のままで、突合そのものを行わない。
           05  WS-COUNTED              PIC X(01) VALUE 'N'.
           05  WS-IN-COUNT OCCURS 4 TIMES PIC 9(05).
      *    -- 当日の現金増減。CASH-PARM は呼出のたびに上書きされる
      *    -- 引数域なので、後で帳票に出す値はここへ退避する。
           05  WS-DISPENSED            PIC S9(13)V99 VALUE ZERO.
           05  WS-DEPOSITED            PIC S9(13)V99 VALUE ZERO.

      *    -- 走査中の 1 レコードを EJ のレイアウトで読むための像
       COPY 'JRNLREC.cpy'.
       COPY 'RECONREC.cpy'.

       COPY 'ATMCONST.cpy'.
       COPY 'RETCODE.cpy'.
       COPY 'ATMSESS.cpy'.
       COPY 'JRNLIF.cpy'.
       COPY 'CASHIF.cpy'.
       COPY 'RPTIF.cpy'.
       COPY 'CLSIF.cpy'.

       01  WS-DATETIME.
           05  WS-CURRENT-DATE.
               10  WS-CD-YYYYMMDD      PIC 9(08).
               10  WS-CD-HHMMSS        PIC 9(06).
               10  FILLER              PIC X(15).

       PROCEDURE DIVISION.

       MAIN-CONTROL SECTION.
       MAIN-START.
           PERFORM INITIALIZE-BATCH
           IF WS-ABORT = 'Y'
               PERFORM FINISH-ABORTED
               STOP RUN
           END-IF

           PERFORM SCAN-JOURNAL
           PERFORM RECONCILE-CASH
           PERFORM WRITE-REPORT
           PERFORM CARRY-FORWARD
           PERFORM FINISH-NORMAL
           STOP RUN.

      *================================================================
      * (1) 締め状態の確認と実行中フラグ
      *================================================================
       INITIALIZE-BATCH SECTION.
       INIT-START.
           INITIALIZE ATM-SESSION
           MOVE CN-ATM-ID TO SESS-ATM-ID
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE WS-CD-YYYYMMDD TO SESS-BUSINESS-DATE
           COMPUTE SESS-TIMESTAMP =
               WS-CD-YYYYMMDD * 1000000 + WS-CD-HHMMSS

           PERFORM INIT-SUMMARY

           DISPLAY ' '
           DISPLAY '=== 日次締め 端末 ' CN-ATM-ID
                   ' 営業日 ' SESS-BUSINESS-DATE ' ==='

           SET CLS-FN-CHECK TO TRUE
           CALL 'ATMCLS' USING CLS-PARM ATM-SESSION
           IF CLS-OUT-RETCODE NOT = RC-OK
               PERFORM SHOW-CLOSE-ERROR
               MOVE 'Y' TO WS-ABORT
               GO TO INIT-EXIT
           END-IF

           SET CLS-FN-START TO TRUE
           CALL 'ATMCLS' USING CLS-PARM ATM-SESSION.
       INIT-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 締められない理由を係員に伝える。判定そのものは ATMCLS が持つ。
      *----------------------------------------------------------------
       SHOW-CLOSE-ERROR SECTION.
       SCE-START.
           EVALUATE CLS-OUT-ERROR-CODE
               WHEN EC-ALREADY-CLOSED
                   DISPLAY '  [' CLS-OUT-ERROR-CODE
                           '] この営業日は締め済みです ('
                           CLS-OUT-LAST-CLOSED ')。'
               WHEN EC-CLOSE-IN-PROGRESS
                   DISPLAY '  [' CLS-OUT-ERROR-CODE
                           '] 締めが実行中です。前回が異常終了した'
                           '場合は係員が解除してください。'
               WHEN OTHER
                   DISPLAY '  [' CLS-OUT-ERROR-CODE
                           '] 締め状態を確認できません。'
           END-EVALUATE.
       SCE-EXIT.
           EXIT.

       INIT-SUMMARY SECTION.
       IS-START.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-TXN-TYPE-CNT
               MOVE ZERO TO WS-SUM-CNT(WS-I)
               MOVE ZERO TO WS-SUM-AMT(WS-I)
           END-PERFORM.
       IS-EXIT.
           EXIT.

      *================================================================
      * (2) EJ の走査
      *   S を見たら未決着表に積み、E が来たら降ろす。走査が終わって
      *   表に残っているものが不確定取引。EJ は時系列順なので 1 パスで
      *   済み、ソートは要らない。
      *================================================================
       SCAN-JOURNAL SECTION.
       SCAN-START.
           SET JRNL-FN-SCAN-OPEN TO TRUE
           CALL 'ATMJRNL' USING JRNL-PARM ATM-SESSION
           IF JRNL-OUT-RETCODE NOT = RC-OK
               DISPLAY '*** EJ を開けません。'
               GO TO SCAN-EXIT
           END-IF

           PERFORM UNTIL JRNL-OUT-EOF = 'Y'
               SET JRNL-FN-SCAN-NEXT TO TRUE
               CALL 'ATMJRNL' USING JRNL-PARM ATM-SESSION
               IF JRNL-OUT-EOF NOT = 'Y'
                   MOVE JRNL-OUT-RECORD TO JRNL-RECORD
                   PERFORM CLASSIFY-JOURNAL-RECORD
               END-IF
           END-PERFORM

           SET JRNL-FN-SCAN-CLOSE TO TRUE
           CALL 'ATMJRNL' USING JRNL-PARM ATM-SESSION

           PERFORM COLLECT-PENDING.
       SCAN-EXIT.
           EXIT.

       CLASSIFY-JOURNAL-RECORD SECTION.
       CJR-START.
      *    -- 当営業日以外のレコードは対象外。EJ は日を跨いで残る。
           IF JRNL-TIMESTAMP (1:8) NOT = SESS-BUSINESS-DATE
               GO TO CJR-EXIT
           END-IF

           EVALUATE JRNL-PHASE
               WHEN 'S' PERFORM PUSH-OPEN-TXN
               WHEN 'E' PERFORM POP-OPEN-TXN
               WHEN 'R' PERFORM CHECK-REVERSAL
           END-EVALUATE.
       CJR-EXIT.
           EXIT.

       PUSH-OPEN-TXN SECTION.
       PUSH-START.
           IF WS-OPEN-CNT >= WS-MAX-OPEN-TXN
               MOVE 'Y' TO WS-OVERFLOW
               GO TO PUSH-EXIT
           END-IF
           ADD 1 TO WS-OPEN-CNT
           MOVE JRNL-TXN-ID TO WS-OPEN-TXN-ID(WS-OPEN-CNT)
           MOVE JRNL-RECORD TO WS-OPEN-RECORD(WS-OPEN-CNT).
       PUSH-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * E が来たら未決着表から降ろす。降ろすと同時に集計もここで取る。
      * 集計は「終わった取引」だけを数える。途中で落ちた取引を売上に
      * 数えると帳簿が合わなくなるため。
      *----------------------------------------------------------------
       POP-OPEN-TXN SECTION.
       POP-START.
           PERFORM FIND-OPEN-TXN
           IF WS-FOUND > ZERO
      *        -- 詰め直さず消費済みの印を付ける。表は集合であって
      *        -- 順序に意味がないため、配列シフトは要らない。
               MOVE SPACES TO WS-OPEN-TXN-ID(WS-FOUND)
           END-IF

           IF JRNL-RESULT = 'S'
               PERFORM ADD-TO-SUMMARY
           END-IF.
       POP-EXIT.
           EXIT.

       FIND-OPEN-TXN SECTION.
       FIND-START.
           MOVE ZERO TO WS-FOUND
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-OPEN-CNT
               IF WS-OPEN-TXN-ID(WS-I) = JRNL-TXN-ID
                   MOVE WS-I TO WS-FOUND
                   EXIT PERFORM
               END-IF
           END-PERFORM.
       FIND-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 取消 (R) レコード。取消自体が失敗していれば勘定が合っていない
      * ので、係員対応として拾う。
      *----------------------------------------------------------------
       CHECK-REVERSAL SECTION.
       CRV-START.
           IF JRNL-RESULT = 'S'
               GO TO CRV-EXIT
           END-IF
           MOVE SPACES TO RECON-RECORD
           SET  RCN-TP-REVERSAL-FAILED TO TRUE
           PERFORM FILL-RECON-FROM-JOURNAL
           PERFORM ADD-RECON.
       CRV-EXIT.
           EXIT.

       ADD-TO-SUMMARY SECTION.
       ATS-START.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-TXN-TYPE-CNT
               IF WS-SUM-TYPE(WS-I) = JRNL-TXN-TYPE
                   ADD 1 TO WS-SUM-CNT(WS-I)
                   ADD JRNL-AMOUNT TO WS-SUM-AMT(WS-I)
                   EXIT PERFORM
               END-IF
           END-PERFORM.
       ATS-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 走査後に残ったものが不確定取引。他行為替で追跡番号が付いて
      * いるものは、相手行に照会すれば成否が判るので種別を分ける。
      *----------------------------------------------------------------
       COLLECT-PENDING SECTION.
       CP-START.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-OPEN-CNT
               IF WS-OPEN-TXN-ID(WS-I) = SPACES
                   EXIT PERFORM CYCLE
               END-IF
               MOVE WS-OPEN-RECORD(WS-I) TO JRNL-RECORD
               MOVE SPACES TO RECON-RECORD
      *        -- 成否不明かどうかは EJ が明示している事実で決める。
      *        -- 追跡番号は正常に着金した振込にも付くので、有無では
      *        -- 判別にならない。
               IF JRNL-ERROR-CODE = EC-ZENGIN-TIMEOUT
                   SET RCN-TP-ZENGIN-UNKNOWN TO TRUE
               ELSE
                   SET RCN-TP-PENDING TO TRUE
               END-IF
               PERFORM FILL-RECON-FROM-JOURNAL
               PERFORM ADD-RECON
               ADD 1 TO WS-PENDING-CNT
           END-PERFORM.
       CP-EXIT.
           EXIT.

       FILL-RECON-FROM-JOURNAL SECTION.
       FRJ-START.
           MOVE JRNL-TXN-ID     TO RCN-TXN-ID
           MOVE JRNL-SESSION-ID TO RCN-SESSION-ID
           MOVE JRNL-TIMESTAMP  TO RCN-TIMESTAMP
           MOVE JRNL-TXN-TYPE   TO RCN-TXN-TYPE
           MOVE JRNL-ACCT-NO    TO RCN-ACCT-NO
           MOVE JRNL-TRACE-NO   TO RCN-TRACE-NO
           MOVE JRNL-AMOUNT     TO RCN-AMOUNT
           MOVE JRNL-ERROR-CODE TO RCN-ERROR-CODE
           MOVE ZERO TO RCN-EXPECTED
           MOVE ZERO TO RCN-ACTUAL.
       FRJ-EXIT.
           EXIT.

       ADD-RECON SECTION.
       AR-START.
           IF WS-RECON-CNT >= WS-MAX-RECON
               MOVE 'Y' TO WS-OVERFLOW
               GO TO AR-EXIT
           END-IF
           ADD 1 TO WS-RECON-CNT
           MOVE RECON-RECORD TO WS-RECON-ENTRY(WS-RECON-CNT).
       AR-EXIT.
           EXIT.

      *================================================================
      * (3) 現金の突合
      *   ATMCASH は帳簿がどうなっているかだけを答える。実査値との
      *   比較はここで行う。判定を現金機構に持たせると、実査の運用が
      *   変わるたびに機構側を直すことになるため。
      *================================================================
       RECONCILE-CASH SECTION.
       RC-START.
           SET CASH-FN-THEORY TO TRUE
           CALL 'ATMCASH' USING CASH-PARM ATM-SESSION
           IF CASH-OUT-RETCODE NOT = RC-OK
               DISPLAY '*** 現金機構の帳簿を読めません。'
               GO TO RC-EXIT
           END-IF

      *    -- 引数域は次の呼出で上書きされるので、帳票に出す値は
      *    -- ここで退避しておく。
           MOVE CASH-OUT-DISPENSED TO WS-DISPENSED
           MOVE CASH-OUT-DEPOSITED TO WS-DEPOSITED

           PERFORM READ-COUNTED-NOTES
           IF WS-COUNTED NOT = 'Y'
               GO TO RC-EXIT
           END-IF

           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > CN-CASSETTE-CNT
               IF WS-IN-COUNT(WS-I) NOT = CASH-TH-CNT(WS-I)
                   MOVE SPACES TO RECON-RECORD
                   SET  RCN-TP-CASH-DIFF TO TRUE
                   MOVE CASH-TH-DENOM(WS-I) TO RCN-AMOUNT
                   MOVE CASH-TH-CNT(WS-I)   TO RCN-EXPECTED
                   MOVE WS-IN-COUNT(WS-I)   TO RCN-ACTUAL
                   MOVE EC-CASH-COUNT-DIFF  TO RCN-ERROR-CODE
                   PERFORM ADD-RECON
                   ADD 1 TO WS-DIFF-CNT
               END-IF
           END-PERFORM.
       RC-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 実査枚数の取得。係員が装置から数えた枚数を入力する経路が
      * まだ無いので、未入力のまま返す。
      *
      * 帳簿値をそのまま実査値として埋めてはいけない。比較が必ず
      * 一致して差異ゼロになり、実査していないのに「実施して問題
      * なし」と読める帳票が出てしまう。未実施は未実施として残す。
      *
      * 係員操作パネルを作る際は、ここで入力値を受け取って
      * WS-COUNTED に 'Y' を立てれば、以降の突合が有効になる。
      *----------------------------------------------------------------
       READ-COUNTED-NOTES SECTION.
       RCN-START.
           MOVE 'N' TO WS-COUNTED.
       RCN-EXIT.
           EXIT.

      *================================================================
      * (4) 帳票
      *================================================================
       WRITE-REPORT SECTION.
       WR-START.
           SET RPT-FN-OPEN TO TRUE
           CALL 'ATMRPT' USING RPT-PARM ATM-SESSION
           IF RPT-OUT-RETCODE NOT = RC-OK
               DISPLAY '*** 帳票を出力できません。'
               GO TO WR-EXIT
           END-IF

           MOVE SESS-BUSINESS-DATE TO RPT-IN-BUSINESS-DATE
           SET RPT-FN-HEADER TO TRUE
           CALL 'ATMRPT' USING RPT-PARM ATM-SESSION

           PERFORM WRITE-SUMMARY-LINES
           PERFORM WRITE-CASH-LINES
           PERFORM WRITE-DETAIL-LINES

           PERFORM DECIDE-ACTION
           MOVE WS-DIFF-CNT    TO RPT-IN-DIFF-CNT
           MOVE WS-PENDING-CNT TO RPT-IN-PENDING-CNT
           MOVE WS-ACTION      TO RPT-IN-ACTION-REQUIRED
           MOVE WS-COUNTED     TO RPT-IN-CASH-COUNTED
           MOVE WS-OVERFLOW    TO RPT-IN-TRUNCATED
           SET RPT-FN-FOOTER TO TRUE
           CALL 'ATMRPT' USING RPT-PARM ATM-SESSION

           SET RPT-FN-CLOSE TO TRUE
           CALL 'ATMRPT' USING RPT-PARM ATM-SESSION.
       WR-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 係員対応が要るかの判断。締めの基準そのものなので、帳票側に
      * 件数から導かせない。検出を打ち切った場合は、件数がゼロでも
      * 記載漏れがあるので要対応とする。
      *----------------------------------------------------------------
       DECIDE-ACTION SECTION.
       DA-START.
           MOVE 'N' TO WS-ACTION
           IF WS-DIFF-CNT > ZERO
              OR WS-PENDING-CNT > ZERO
              OR WS-OVERFLOW = 'Y'
               MOVE 'Y' TO WS-ACTION
           END-IF.
       DA-EXIT.
           EXIT.

       WRITE-SUMMARY-LINES SECTION.
       WSL-START.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-TXN-TYPE-CNT
               MOVE WS-SUM-TYPE(WS-I) TO RPT-IN-LABEL
               MOVE WS-SUM-CNT(WS-I) TO RPT-IN-COUNT
               MOVE WS-SUM-AMT(WS-I) TO RPT-IN-AMOUNT
               SET RPT-FN-SUMMARY TO TRUE
               CALL 'ATMRPT' USING RPT-PARM ATM-SESSION
           END-PERFORM.
       WSL-EXIT.
           EXIT.

      *    -- 現金の動き。金種別の残枚数ではなく当日の増減を出す。
      *    -- 枚数は差異があったときだけ明細に出る。
       WRITE-CASH-LINES SECTION.
       WCL-START.
           MOVE '払出額 (当日)' TO RPT-IN-LABEL
           MOVE WS-DISPENSED    TO RPT-IN-AMOUNT
           PERFORM WRITE-AMOUNT-LINE

           MOVE '収納額 (当日)' TO RPT-IN-LABEL
           MOVE WS-DEPOSITED    TO RPT-IN-AMOUNT
           PERFORM WRITE-AMOUNT-LINE.
       WCL-EXIT.
           EXIT.

      *    -- 件数を持たない金額だけの集計行
       WRITE-AMOUNT-LINE SECTION.
       WAL-START.
           MOVE ZERO TO RPT-IN-COUNT
           SET RPT-FN-SUMMARY TO TRUE
           CALL 'ATMRPT' USING RPT-PARM ATM-SESSION.
       WAL-EXIT.
           EXIT.

       WRITE-DETAIL-LINES SECTION.
       WDL-START.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-RECON-CNT
               MOVE WS-RECON-ENTRY(WS-I) TO RPT-IN-RECON
               SET RPT-FN-DETAIL TO TRUE
               CALL 'ATMRPT' USING RPT-PARM ATM-SESSION
           END-PERFORM.
       WDL-EXIT.
           EXIT.

      *================================================================
      * (5) 繰越。検出がすべて終わってから当日計を消す。
      *================================================================
       CARRY-FORWARD SECTION.
       CF-START.
           SET CASH-FN-SETTLE TO TRUE
           CALL 'ATMCASH' USING CASH-PARM ATM-SESSION
           IF CASH-OUT-RETCODE NOT = RC-OK
               DISPLAY '*** 当日計を繰り越せません。'
           END-IF

           SET CASH-FN-CLOSE TO TRUE
           CALL 'ATMCASH' USING CASH-PARM ATM-SESSION

           PERFORM ARCHIVE-JOURNAL.
       CF-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * EJ の退避。当日分を日付つきのファイルへ写し、現用の EJ を
      * 空にする。追記専用のまま伸ばし続けると、起動時の通番復元と
      * 締めの走査が運用日数に比例して重くなる。
      *
      * 帳票を書き終えてから行う。写しに失敗しても当日の締め結果は
      * 残り、EJ も現用のまま手つかずで残るので、原因を解いてから
      * やり直せる。
      *----------------------------------------------------------------
       ARCHIVE-JOURNAL SECTION.
       AJ-START.
           MOVE SESS-BUSINESS-DATE TO JRNL-IN-ARCHIVE-DATE
           SET JRNL-FN-ARCHIVE TO TRUE
           CALL 'ATMJRNL' USING JRNL-PARM ATM-SESSION

           IF JRNL-OUT-RETCODE = RC-OK
               DISPLAY '  EJ 退避  : ' JRNL-OUT-ARCHIVED-CNT ' 件'
           ELSE
               DISPLAY '*** EJ を退避できませんでした。'
                       ' 現用の EJ はそのまま残しています。'
               MOVE 'Y' TO WS-ACTION
           END-IF.
       AJ-EXIT.
           EXIT.

       FINISH-NORMAL SECTION.
       FN-START.
           MOVE WS-DIFF-CNT    TO CLS-IN-DIFF-CNT
           MOVE WS-PENDING-CNT TO CLS-IN-PENDING-CNT
           SET  CLS-FN-FINISH TO TRUE
           CALL 'ATMCLS' USING CLS-PARM ATM-SESSION
           SET  CLS-FN-CLOSE TO TRUE
           CALL 'ATMCLS' USING CLS-PARM ATM-SESSION

           DISPLAY '  取引集計 : ' WS-SUM-CNT(2) ' 出金 / '
                   WS-SUM-CNT(3) ' 入金 / ' WS-SUM-CNT(4) ' 振込'
           DISPLAY '  不確定取引: ' WS-PENDING-CNT ' 件'
           IF WS-COUNTED = 'Y'
               DISPLAY '  現金差異  : ' WS-DIFF-CNT ' 件'
           ELSE
               DISPLAY '  現金実査  : 未実施 (実査枚数の入力経路なし)'
           END-IF
           IF WS-OVERFLOW = 'Y'
               DISPLAY '*** 検出件数が上限に達し、明細を打ち切りました。'
           END-IF
           IF WS-ACTION = 'Y'
               DISPLAY '*** 係員の確認が必要です。'
                       ' 帳票 data/atmrpt.txt を参照してください。'
           ELSE
               DISPLAY '  締め処理を完了しました。'
           END-IF.
       FN-EXIT.
           EXIT.

      *    -- 締められなかった場合。実行中フラグは立てていないので
      *    -- 状態はそのまま。係員が原因を解いてから再実行する。
       FINISH-ABORTED SECTION.
       FA-START.
           SET CLS-FN-CLOSE TO TRUE
           CALL 'ATMCLS' USING CLS-PARM ATM-SESSION
           DISPLAY '  締め処理を行いませんでした。'.
       FA-EXIT.
           EXIT.

       END PROGRAM ATMDAY.
