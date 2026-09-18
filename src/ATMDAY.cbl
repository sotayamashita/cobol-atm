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

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLOSE-FILE ASSIGN TO 'data/atmclose.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CLS-ATM-ID
               FILE STATUS IS WS-CLOSE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CLOSE-FILE.
       COPY 'CLOSEREC.cpy'.

       WORKING-STORAGE SECTION.
       01  WS-CLOSE-STATUS             PIC X(02) VALUE '00'.

       01  WS-CONST.
      *    -- 未決着の取引を保持する上限。1 営業日の取引数を超える
      *    -- ことはないが、溢れた場合は帳票で明示する。
           05  WS-MAX-OPEN-TXN         PIC S9(04) COMP VALUE 500.
           05  WS-MAX-RECON            PIC S9(04) COMP VALUE 500.

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
       01  WS-SUMMARY-TABLE.
           05  WS-SUM-ENTRY OCCURS 5 TIMES.
               10  WS-SUM-TYPE         PIC X(02).
               10  WS-SUM-CNT          PIC 9(07).
               10  WS-SUM-AMT          PIC S9(13)V99.

       01  WS-WORK.
           05  WS-I                    PIC S9(04) COMP VALUE ZERO.
           05  WS-J                    PIC S9(04) COMP VALUE ZERO.
           05  WS-FOUND                PIC S9(04) COMP VALUE ZERO.
           05  WS-DIFF-CNT             PIC 9(05) VALUE ZERO.
           05  WS-PENDING-CNT          PIC 9(05) VALUE ZERO.
           05  WS-OVERFLOW             PIC X(01) VALUE 'N'.
           05  WS-ABORT                PIC X(01) VALUE 'N'.
      *    -- 実査枚数。係員入力の口。現状は帳簿値で埋める。
           05  WS-IN-COUNT OCCURS 4 TIMES PIC 9(05).
      *    -- EJ の日付部。除算の結果をそのまま比較すると小数が残って
      *    -- 一致しないため、整数項目へ落としてから突き合わせる。
           05  WS-JRNL-DATE            PIC 9(08) VALUE ZERO.

      *    -- 走査中の 1 レコードを EJ のレイアウトで読むための像
       COPY 'JRNLREC.cpy'.
       COPY 'RECONREC.cpy'.

       COPY 'ATMCONST.cpy'.
       COPY 'RETCODE.cpy'.
       COPY 'ATMSESS.cpy'.
       COPY 'JRNLIF.cpy'.
       COPY 'CASHIF.cpy'.
       COPY 'RPTIF.cpy'.
       COPY 'CALIF.cpy'.

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

           OPEN I-O CLOSE-FILE
           IF WS-CLOSE-STATUS NOT = '00'
               DISPLAY '*** 締め状態を読めません。中止します。'
               MOVE 'Y' TO WS-ABORT
               GO TO INIT-EXIT
           END-IF

           MOVE CN-ATM-ID TO CLS-ATM-ID
           READ CLOSE-FILE
               INVALID KEY
                   DISPLAY '*** 締め状態が未登録です。中止します。'
                   MOVE 'Y' TO WS-ABORT
                   GO TO INIT-EXIT
           END-READ

           PERFORM CHECK-CLOSABLE
           IF WS-ABORT = 'Y'
               GO TO INIT-EXIT
           END-IF

           SET CLS-ST-RUNNING TO TRUE
           REWRITE CLOSE-RECORD
           END-REWRITE.
       INIT-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 締めてよい状態かを見る。同じ営業日の再実行と、前回の異常終了
      * が残っている状態を弾く。どちらも自動で進めてはいけない。
      *----------------------------------------------------------------
       CHECK-CLOSABLE SECTION.
       CHK-START.
           EVALUATE TRUE
               WHEN CLS-ST-RUNNING
                   DISPLAY '  [' EC-CLOSE-IN-PROGRESS
                           '] 締めが実行中です。'
                           '前回が異常終了した場合は係員が解除してください。'
                   MOVE 'Y' TO WS-ABORT
               WHEN CLS-ST-ABORTED
                   DISPLAY '  [' EC-CLOSE-ABORTED
                           '] 前回の締めが中断しています。'
                           '係員の確認が必要です。'
                   MOVE 'Y' TO WS-ABORT
               WHEN CLS-LAST-CLOSED-DATE >= SESS-BUSINESS-DATE
                   DISPLAY '  [' EC-ALREADY-CLOSED
                           '] この営業日は締め済みです ('
                           CLS-LAST-CLOSED-DATE ')。'
                   MOVE 'Y' TO WS-ABORT
           END-EVALUATE.
       CHK-EXIT.
           EXIT.

       INIT-SUMMARY SECTION.
       IS-START.
           MOVE 'IQ' TO WS-SUM-TYPE(1)
           MOVE 'WD' TO WS-SUM-TYPE(2)
           MOVE 'DP' TO WS-SUM-TYPE(3)
           MOVE 'TR' TO WS-SUM-TYPE(4)
           MOVE 'PC' TO WS-SUM-TYPE(5)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
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
           COMPUTE WS-JRNL-DATE = JRNL-TIMESTAMP / 1000000
           IF WS-JRNL-DATE NOT = SESS-BUSINESS-DATE
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
               PERFORM REMOVE-OPEN-TXN
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

       REMOVE-OPEN-TXN SECTION.
       REM-START.
           PERFORM VARYING WS-J FROM WS-FOUND BY 1
                   UNTIL WS-J >= WS-OPEN-CNT
               MOVE WS-OPEN-TXN-ID(WS-J + 1) TO WS-OPEN-TXN-ID(WS-J)
               MOVE WS-OPEN-RECORD(WS-J + 1) TO WS-OPEN-RECORD(WS-J)
           END-PERFORM
           SUBTRACT 1 FROM WS-OPEN-CNT.
       REM-EXIT.
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
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
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
               MOVE WS-OPEN-RECORD(WS-I) TO JRNL-RECORD
               MOVE SPACES TO RECON-RECORD
               IF JRNL-TRACE-NO NOT = SPACES
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

           PERFORM READ-COUNTED-NOTES

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
      * 実査枚数の取得。本来は係員が装置から数えた枚数を入力する。
      * その経路がまだ無いので帳簿値をそのまま使い、差異ゼロになる。
      * 係員操作パネルを作る際はここを差し替える。
      *----------------------------------------------------------------
       READ-COUNTED-NOTES SECTION.
       RCN-START.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > CN-CASSETTE-CNT
               MOVE CASH-TH-CNT(WS-I) TO WS-IN-COUNT(WS-I)
           END-PERFORM.
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

           MOVE WS-DIFF-CNT    TO RPT-IN-DIFF-CNT
           MOVE WS-PENDING-CNT TO RPT-IN-PENDING-CNT
           SET RPT-FN-FOOTER TO TRUE
           CALL 'ATMRPT' USING RPT-PARM ATM-SESSION

           SET RPT-FN-CLOSE TO TRUE
           CALL 'ATMRPT' USING RPT-PARM ATM-SESSION.
       WR-EXIT.
           EXIT.

       WRITE-SUMMARY-LINES SECTION.
       WSL-START.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               PERFORM SET-SUMMARY-LABEL
               MOVE WS-SUM-CNT(WS-I) TO RPT-IN-COUNT
               MOVE WS-SUM-AMT(WS-I) TO RPT-IN-AMOUNT
               SET RPT-FN-SUMMARY TO TRUE
               CALL 'ATMRPT' USING RPT-PARM ATM-SESSION
           END-PERFORM.
       WSL-EXIT.
           EXIT.

       SET-SUMMARY-LABEL SECTION.
       SSL-START.
           EVALUATE WS-SUM-TYPE(WS-I)
               WHEN 'IQ' MOVE '残高照会'     TO RPT-IN-LABEL
               WHEN 'WD' MOVE 'お引出し'     TO RPT-IN-LABEL
               WHEN 'DP' MOVE 'お預入れ'     TO RPT-IN-LABEL
               WHEN 'TR' MOVE 'お振込み'     TO RPT-IN-LABEL
               WHEN 'PC' MOVE '暗証番号変更' TO RPT-IN-LABEL
               WHEN OTHER MOVE SPACES        TO RPT-IN-LABEL
           END-EVALUATE.
       SSL-EXIT.
           EXIT.

      *    -- 現金の動き。金種別の残枚数ではなく当日の増減を出す。
      *    -- 枚数は差異があったときだけ明細に出る。
       WRITE-CASH-LINES SECTION.
       WCL-START.
           MOVE '払出額 (当日)' TO RPT-IN-LABEL
           MOVE ZERO                 TO RPT-IN-COUNT
           MOVE CASH-OUT-DISPENSED   TO RPT-IN-AMOUNT
           SET RPT-FN-SUMMARY TO TRUE
           CALL 'ATMRPT' USING RPT-PARM ATM-SESSION

           MOVE '収納額 (当日)' TO RPT-IN-LABEL
           MOVE ZERO                 TO RPT-IN-COUNT
           MOVE CASH-OUT-DEPOSITED   TO RPT-IN-AMOUNT
           SET RPT-FN-SUMMARY TO TRUE
           CALL 'ATMRPT' USING RPT-PARM ATM-SESSION.
       WCL-EXIT.
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
           CALL 'ATMCASH' USING CASH-PARM ATM-SESSION.
       CF-EXIT.
           EXIT.

       FINISH-NORMAL SECTION.
       FN-START.
           MOVE SESS-BUSINESS-DATE TO CLS-LAST-CLOSED-DATE
           MOVE SESS-TIMESTAMP     TO CLS-LAST-CLOSED-TS
           MOVE WS-DIFF-CNT        TO CLS-LAST-DIFF-CNT
           MOVE WS-PENDING-CNT     TO CLS-LAST-PENDING-CNT
           SET  CLS-ST-IDLE TO TRUE
           REWRITE CLOSE-RECORD
           END-REWRITE
           CLOSE CLOSE-FILE

           DISPLAY '  取引集計 : ' WS-SUM-CNT(2) ' 出金 / '
                   WS-SUM-CNT(3) ' 入金 / ' WS-SUM-CNT(4) ' 振込'
           DISPLAY '  不確定取引: ' WS-PENDING-CNT ' 件'
           DISPLAY '  現金差異  : ' WS-DIFF-CNT ' 件'
           IF WS-OVERFLOW = 'Y'
               DISPLAY '*** 検出件数が上限を超えました。'
                       '帳票は一部のみです。'
           END-IF
           IF WS-DIFF-CNT > ZERO OR WS-PENDING-CNT > ZERO
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
           IF WS-CLOSE-STATUS = '00'
               CLOSE CLOSE-FILE
           END-IF
           DISPLAY '  締め処理を行いませんでした。'.
       FA-EXIT.
           EXIT.

       END PROGRAM ATMDAY.
