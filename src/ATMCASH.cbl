      *****************************************************************
      * PROGRAM : ATMCASH
      * PURPOSE : 現金機構 (紙幣払出・入金収納・カセット在庫) 制御
      * DESIGN  :
      *   金種計算は「貪欲法」では不成立ケース (例: 万券切れで 2千券と
      *   千券だけ残る状況) を取りこぼすため、有界ナップサック DP で
      *   最小枚数解を厳密に求める。
      *     状態  F(c,u) = カセット 1..c のみを使って u (千円単位) を
      *                    構成する最小枚数。不能は INF。
      *     遷移  F(c,u) = min{ F(c-1, u - k*d(c)) + k }  (0<=k<=avail(c))
      *     復元  K(c,u) に採用した k を保存し、c=N から逆順に辿る。
      *   PLAN は在庫を減らさない (照会のみ)。実際の減算は DISPENSE。
      *   PLAN と DISPENSE を分けるのは、記帳成功後に初めて紙幣を
      *   繰り出すという順序を守るため。
      *   入金は金種別計数機が数えた内訳をそのまま在庫へ加算する。
      *   金額からの推定はしない (DO-ACCEPT を参照)。
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMCASH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CASH-FILE ASSIGN TO 'data/atmcash.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CASH-ATM-ID
               FILE STATUS IS WS-CASH-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CASH-FILE.
       COPY 'CASHREC.cpy'.

       WORKING-STORAGE SECTION.
       01  WS-CASH-STATUS              PIC X(02) VALUE '00'.
       01  WS-OPENED                   PIC X(01) VALUE 'N'.

       COPY 'ATMCONST.cpy'.

       01  WS-CONST.
           05  WS-MAX-UNITS            PIC 9(04) VALUE 1000.
           05  WS-INFINITY             PIC 9(05) VALUE 99999.

       01  WS-WORK.
           05  WS-C                    PIC S9(04) COMP VALUE ZERO.
           05  WS-U                    PIC S9(04) COMP VALUE ZERO.
           05  WS-K                    PIC S9(04) COMP VALUE ZERO.
           05  WS-KMAX                 PIC S9(04) COMP VALUE ZERO.
           05  WS-PREV-U               PIC S9(04) COMP VALUE ZERO.
           05  WS-CAND                 PIC S9(05) COMP VALUE ZERO.
           05  WS-TARGET-UNITS         PIC S9(04) COMP VALUE ZERO.
           05  WS-DENOM-UNITS          PIC S9(04) COMP OCCURS 4 TIMES.
           05  WS-AVAIL                PIC S9(05) COMP OCCURS 4 TIMES.
           05  WS-PLAN-CNT             PIC S9(05) COMP OCCURS 4 TIMES.
           05  WS-REMAIN               PIC S9(13)V99 VALUE ZERO.
           05  WS-AMT-INT              PIC S9(13) VALUE ZERO.
           05  WS-DEP-SUM              PIC S9(13)V99 VALUE ZERO.

      *    -- DP 表。添字は c=1..5 (1 が c=0 に相当), u=1..1001 (1 が u=0)
       01  WS-DP-TABLE.
           05  WS-DP-ROW OCCURS 5 TIMES.
               10  WS-F    OCCURS 1001 TIMES PIC 9(05) COMP.
               10  WS-KSEL OCCURS 1001 TIMES PIC 9(05) COMP.

       COPY 'RETCODE.cpy'.

       LINKAGE SECTION.
       COPY 'CASHIF.cpy'.
       COPY 'ATMSESS.cpy'.

       PROCEDURE DIVISION USING CASH-PARM ATM-SESSION.

       MAIN-CONTROL SECTION.
       MAIN-START.
           MOVE RC-OK   TO CASH-OUT-RETCODE
           MOVE EC-NONE TO CASH-OUT-ERROR-CODE

           EVALUATE TRUE
               WHEN CASH-FN-OPEN      PERFORM OPEN-CASH
               WHEN CASH-FN-PLAN      PERFORM PLAN-DISPENSE
               WHEN CASH-FN-DISPENSE  PERFORM DO-DISPENSE
               WHEN CASH-FN-ACCEPT    PERFORM DO-ACCEPT
               WHEN CASH-FN-CLOSE     PERFORM CLOSE-CASH
               WHEN CASH-FN-THEORY    PERFORM REPORT-THEORY
               WHEN CASH-FN-SETTLE    PERFORM DO-SETTLE
               WHEN OTHER
                   MOVE RC-FATAL TO CASH-OUT-RETCODE
           END-EVALUATE
           GOBACK.

       OPEN-CASH SECTION.
       OPEN-C-START.
           IF WS-OPENED = 'Y'
               GO TO OPEN-C-EXIT
           END-IF
           OPEN I-O CASH-FILE
           IF WS-CASH-STATUS = '00'
               MOVE 'Y' TO WS-OPENED
           ELSE
               MOVE RC-IO-ERROR  TO CASH-OUT-RETCODE
               MOVE EC-SYSTEM-IO TO CASH-OUT-ERROR-CODE
               GO TO OPEN-C-EXIT
           END-IF

      *    -- 金種の並びは据付構成で取引ごとに変わらない。開局時に一度
      *    -- 返しておけば、呼出元が取引のたびに在庫を読み直さずに済む。
           PERFORM LOAD-CASSETTE
           IF CASH-OUT-RETCODE NOT = RC-OK
               GO TO OPEN-C-EXIT
           END-IF
           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > CN-CASSETTE-CNT
               MOVE CASH-DENOM(WS-C) TO CASH-LO-DENOM(WS-C)
           END-PERFORM.
       OPEN-C-EXIT.
           EXIT.

       LOAD-CASSETTE SECTION.
       LOAD-START.
           IF WS-OPENED NOT = 'Y'
               PERFORM OPEN-CASH
               IF CASH-OUT-RETCODE NOT = RC-OK
                   GO TO LOAD-EXIT
               END-IF
           END-IF
           MOVE SESS-ATM-ID TO CASH-ATM-ID
           READ CASH-FILE
               INVALID KEY
                   MOVE RC-IO-ERROR  TO CASH-OUT-RETCODE
                   MOVE EC-SYSTEM-IO TO CASH-OUT-ERROR-CODE
           END-READ.
       LOAD-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * PLAN : 金種構成を DP で求める。在庫は変更しない。
      *----------------------------------------------------------------
       PLAN-DISPENSE SECTION.
       PLAN-START.
           PERFORM LOAD-CASSETTE
           IF CASH-OUT-RETCODE NOT = RC-OK
               GO TO PLAN-EXIT
           END-IF

           MOVE ZERO TO CASH-PL-DENOM(1) CASH-PL-DENOM(2)
                        CASH-PL-DENOM(3) CASH-PL-DENOM(4)
                        CASH-PL-CNT(1)   CASH-PL-CNT(2)
                        CASH-PL-CNT(3)   CASH-PL-CNT(4)

      *    -- 最小取扱単位の検証 (端数 1 円でも払出不能とする)
           PERFORM CHECK-UNIT
           IF CASH-OUT-RETCODE NOT = RC-OK
               GO TO PLAN-EXIT
           END-IF

           COMPUTE WS-TARGET-UNITS = WS-AMT-INT / CN-CASH-UNIT
           IF WS-TARGET-UNITS <= ZERO OR WS-TARGET-UNITS > WS-MAX-UNITS
               MOVE RC-BUSINESS-ERROR  TO CASH-OUT-RETCODE
               MOVE EC-AMOUNT-INVALID  TO CASH-OUT-ERROR-CODE
               GO TO PLAN-EXIT
           END-IF

           PERFORM COMPUTE-AVAILABLE
           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > CN-CASSETTE-CNT
               COMPUTE WS-DENOM-UNITS(WS-C) = CASH-DENOM(WS-C) / CN-CASH-UNIT
           END-PERFORM

           PERFORM SOLVE-DP
           IF CASH-OUT-RETCODE NOT = RC-OK
               GO TO PLAN-EXIT
           END-IF

           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > CN-CASSETTE-CNT
               MOVE CASH-DENOM(WS-C)  TO CASH-PL-DENOM(WS-C)
               MOVE WS-PLAN-CNT(WS-C) TO CASH-PL-CNT(WS-C)
           END-PERFORM.
       PLAN-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * DP 本体 + 解の復元
      *----------------------------------------------------------------
       SOLVE-DP SECTION.
       DP-START.
      *    -- 初期化 (c=0 の行)
           PERFORM VARYING WS-U FROM 1 BY 1
                   UNTIL WS-U > WS-TARGET-UNITS + 1
               MOVE WS-INFINITY TO WS-F(1, WS-U)
               MOVE ZERO        TO WS-KSEL(1, WS-U)
           END-PERFORM
           MOVE ZERO TO WS-F(1, 1)

           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > CN-CASSETTE-CNT
               PERFORM VARYING WS-U FROM 1 BY 1
                       UNTIL WS-U > WS-TARGET-UNITS + 1
                   MOVE WS-INFINITY TO WS-F(WS-C + 1, WS-U)
                   MOVE ZERO        TO WS-KSEL(WS-C + 1, WS-U)

      *            -- k の上限は在庫と金額の両方で決まる
                   COMPUTE WS-KMAX = (WS-U - 1) / WS-DENOM-UNITS(WS-C)
                   IF WS-KMAX > WS-AVAIL(WS-C)
                       MOVE WS-AVAIL(WS-C) TO WS-KMAX
                   END-IF

                   PERFORM VARYING WS-K FROM 0 BY 1 UNTIL WS-K > WS-KMAX
                       COMPUTE WS-PREV-U =
                           WS-U - WS-K * WS-DENOM-UNITS(WS-C)
                       IF WS-F(WS-C, WS-PREV-U) < WS-INFINITY
                           COMPUTE WS-CAND = WS-F(WS-C, WS-PREV-U) + WS-K
                           IF WS-CAND < WS-F(WS-C + 1, WS-U)
                               MOVE WS-CAND TO WS-F(WS-C + 1, WS-U)
                               MOVE WS-K    TO WS-KSEL(WS-C + 1, WS-U)
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM

           IF WS-F(CN-CASSETTE-CNT + 1, WS-TARGET-UNITS + 1)
              >= WS-INFINITY
      *        -- 在庫総額が足りないのか、金種の組合せが無いのかを区別
               PERFORM JUDGE-INFEASIBLE-REASON
               GO TO DP-EXIT
           END-IF

      *    -- 解の復元
           COMPUTE WS-U = WS-TARGET-UNITS + 1
           PERFORM VARYING WS-C FROM CN-CASSETTE-CNT BY -1
                   UNTIL WS-C < 1
               MOVE WS-KSEL(WS-C + 1, WS-U) TO WS-PLAN-CNT(WS-C)
               COMPUTE WS-U =
                   WS-U - WS-PLAN-CNT(WS-C) * WS-DENOM-UNITS(WS-C)
           END-PERFORM.
       DP-EXIT.
           EXIT.

       JUDGE-INFEASIBLE-REASON SECTION.
       JUDGE-START.
           MOVE ZERO TO WS-REMAIN
           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > CN-CASSETTE-CNT
               COMPUTE WS-REMAIN =
                   WS-REMAIN + WS-AVAIL(WS-C) * CASH-DENOM(WS-C)
           END-PERFORM

           MOVE RC-BUSINESS-ERROR TO CASH-OUT-RETCODE
           IF WS-REMAIN < CASH-IN-AMOUNT
               MOVE EC-CASH-SHORTAGE TO CASH-OUT-ERROR-CODE
           ELSE
               MOVE EC-CASH-NO-COMBINATION TO CASH-OUT-ERROR-CODE
           END-IF.
       JUDGE-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * DISPENSE : PLAN 結果どおりに在庫を減算し、繰出を確定する
      *----------------------------------------------------------------
       DO-DISPENSE SECTION.
       DSP-START.
           PERFORM LOAD-CASSETTE
           IF CASH-OUT-RETCODE NOT = RC-OK
               GO TO DSP-EXIT
           END-IF

      *    -- PLAN 後に他取引で在庫が動いた、あるいはカセットが障害に
      *    -- 落ちた可能性があるため、同じ可用枚数の定義で再検証する
           PERFORM COMPUTE-AVAILABLE
           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > CN-CASSETTE-CNT
               IF CASH-PL-CNT(WS-C) > WS-AVAIL(WS-C)
                   MOVE RC-BUSINESS-ERROR TO CASH-OUT-RETCODE
                   MOVE EC-CASH-SHORTAGE  TO CASH-OUT-ERROR-CODE
                   GO TO DSP-EXIT
               END-IF
           END-PERFORM

           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > CN-CASSETTE-CNT
               SUBTRACT CASH-PL-CNT(WS-C) FROM CASH-NOTE-CNT(WS-C)
               MOVE CASH-PL-CNT(WS-C) TO SESS-DSP-CNT(WS-C)
               MOVE CASH-DENOM(WS-C)  TO SESS-DSP-DENOM(WS-C)
           END-PERFORM

           ADD CASH-IN-AMOUNT TO CASH-DISPENSED-TODAY
           PERFORM REFRESH-CASSETTE-STATUS
           PERFORM SAVE-CASSETTE.
       DSP-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * ACCEPT : 入金収納。金種別計数機が数えた内訳 (CASH-IN-DEPOSIT)
      *          どおりに在庫へ加算する。
      *   投入金額から金種を推定してはならない。推定は「万券から詰める」
      *   といった仮定を置くことになり、実際に入った紙幣と在庫の金種
      *   内訳がずれる。ずれると締めの現金突合が成立しない。
      *----------------------------------------------------------------
       DO-ACCEPT SECTION.
       ACC-START.
           PERFORM LOAD-CASSETTE
           IF CASH-OUT-RETCODE NOT = RC-OK
               GO TO ACC-EXIT
           END-IF

           PERFORM CHECK-UNIT
           IF CASH-OUT-RETCODE NOT = RC-OK
               GO TO ACC-EXIT
           END-IF

      *    -- 在庫へ加算する前に内訳を全件検証する。検証しながら
      *    -- 加算すると、途中で弾いたときに一部だけ増えた在庫が残る。
           PERFORM VALIDATE-DEPOSIT-DETAIL
           IF CASH-OUT-RETCODE NOT = RC-OK
               GO TO ACC-EXIT
           END-IF

           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > CN-CASSETTE-CNT
               ADD CASH-DP-CNT(WS-C)   TO CASH-NOTE-CNT(WS-C)
               MOVE CASH-DP-CNT(WS-C)   TO SESS-DEP-CNT(WS-C)
               MOVE CASH-DENOM(WS-C)    TO SESS-DEP-DENOM(WS-C)
           END-PERFORM

           ADD CASH-IN-AMOUNT TO CASH-DEPOSITED-TODAY
           PERFORM REFRESH-CASSETTE-STATUS
           PERFORM SAVE-CASSETTE.
       ACC-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 入金内訳の検証。内訳の添字はカセット番号と一致する約束なので
      * (CASHIF.cpy)、金種が同じ位置で揃っているかを見れば足りる。
      * 揃っていなければ呼出元が別の並びで渡しており、そのまま加算すると
      * 在庫が壊れる。合計が記帳額と一致することまで確かめてから返す。
      *----------------------------------------------------------------
       VALIDATE-DEPOSIT-DETAIL SECTION.
       VDD-START.
           MOVE ZERO TO WS-DEP-SUM
           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > CN-CASSETTE-CNT
               IF CASH-DP-CNT(WS-C) > ZERO
                   IF CASH-DP-DENOM(WS-C) NOT = CASH-DENOM(WS-C)
                       MOVE RC-BUSINESS-ERROR      TO CASH-OUT-RETCODE
                       MOVE EC-CASH-DEPOSIT-DETAIL TO
                            CASH-OUT-ERROR-CODE
                       GO TO VDD-EXIT
                   END-IF
                   COMPUTE WS-DEP-SUM = WS-DEP-SUM
                       + CASH-DP-CNT(WS-C) * CASH-DP-DENOM(WS-C)
               END-IF
           END-PERFORM

      *    -- 記帳額と収納額の一致は在庫と元帳の整合そのもの。
           IF WS-DEP-SUM NOT = CASH-IN-AMOUNT
               MOVE RC-BUSINESS-ERROR      TO CASH-OUT-RETCODE
               MOVE EC-CASH-DEPOSIT-DETAIL TO CASH-OUT-ERROR-CODE
           END-IF.
       VDD-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * THEORY : 帳簿上あるべき枚数と当日の増減を返す。照会のみで在庫は
      *          一切変更しない (SAVE-CASSETTE を呼ばない)。
      *   実査枚数との突合と過不足判定は締めバッチ側の責務とする。
      *   実査値の入力元・許容差・再計数の運用ルールは現場ごとに異なり、
      *   ここに持ち込むと現金機構の制御と締め運用が癒着するため、
      *   このモジュールは「帳簿がどうなっているか」だけを答える。
      *   払出可能枚数 (COMPUTE-AVAILABLE) ではなく CASH-NOTE-CNT を
      *   そのまま返すのは、障害中カセットの紙幣も物理的には残っており
      *   帳簿上は在庫だからである。
      *----------------------------------------------------------------
       REPORT-THEORY SECTION.
       THR-START.
           PERFORM LOAD-CASSETTE
           IF CASH-OUT-RETCODE NOT = RC-OK
               GO TO THR-EXIT
           END-IF

           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > CN-CASSETTE-CNT
               MOVE CASH-DENOM(WS-C)    TO CASH-TH-DENOM(WS-C)
               MOVE CASH-NOTE-CNT(WS-C) TO CASH-TH-CNT(WS-C)
           END-PERFORM

           MOVE CASH-DISPENSED-TODAY TO CASH-OUT-DISPENSED
           MOVE CASH-DEPOSITED-TODAY TO CASH-OUT-DEPOSITED.
       THR-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * SETTLE : 当日計をクリアし営業日を繰り越す。
      *   枚数 (CASH-NOTE-CNT) は触らない。締めで実際の紙幣は動かず、
      *   在庫は翌営業日にそのまま引き継がれるため。
      *   状態は枚数据置きでも整合させるべき値なので、既存の
      *   REFRESH-CASSETTE-STATUS を通してから書き戻す。
      *----------------------------------------------------------------
       DO-SETTLE SECTION.
       STL-START.
           PERFORM LOAD-CASSETTE
           IF CASH-OUT-RETCODE NOT = RC-OK
               GO TO STL-EXIT
           END-IF

           MOVE ZERO TO CASH-DISPENSED-TODAY
                        CASH-DEPOSITED-TODAY
           MOVE SESS-BUSINESS-DATE TO CASH-BUSINESS-DATE

           PERFORM REFRESH-CASSETTE-STATUS
           PERFORM SAVE-CASSETTE.
       STL-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 共通: カセットごとの払出可能枚数。障害中 (F) と空 (E) は在庫
      *       ゼロとみなす。この定義を PLAN と DISPENSE で共有する。
      *----------------------------------------------------------------
       COMPUTE-AVAILABLE SECTION.
       AVL-START.
           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > CN-CASSETTE-CNT
               IF CASH-ST-FAULT(WS-C) OR CASH-ST-EMPTY(WS-C)
                   MOVE ZERO TO WS-AVAIL(WS-C)
               ELSE
                   MOVE CASH-NOTE-CNT(WS-C) TO WS-AVAIL(WS-C)
               END-IF
           END-PERFORM.
       AVL-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 共通: 取扱単位 (千円) の検証。円未満・千円未満の端数を弾く。
      *----------------------------------------------------------------
       CHECK-UNIT SECTION.
       CHKU-START.
           MOVE CASH-IN-AMOUNT TO WS-AMT-INT
           IF WS-AMT-INT NOT = CASH-IN-AMOUNT
               MOVE RC-BUSINESS-ERROR  TO CASH-OUT-RETCODE
               MOVE EC-AMOUNT-NOT-UNIT TO CASH-OUT-ERROR-CODE
               GO TO CHKU-EXIT
           END-IF
           IF FUNCTION MOD (WS-AMT-INT, CN-CASH-UNIT) NOT = ZERO
               MOVE RC-BUSINESS-ERROR  TO CASH-OUT-RETCODE
               MOVE EC-AMOUNT-NOT-UNIT TO CASH-OUT-ERROR-CODE
           END-IF.
       CHKU-EXIT.
           EXIT.

       REFRESH-CASSETTE-STATUS SECTION.
       REF-START.
           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > CN-CASSETTE-CNT
               IF NOT CASH-ST-FAULT(WS-C)
                   EVALUATE TRUE
                       WHEN CASH-NOTE-CNT(WS-C) = ZERO
                           SET CASH-ST-EMPTY(WS-C) TO TRUE
                       WHEN CASH-NOTE-CNT(WS-C) <= CASH-LOW-WATER(WS-C)
                           SET CASH-ST-LOW(WS-C) TO TRUE
                       WHEN OTHER
                           SET CASH-ST-OK(WS-C) TO TRUE
                   END-EVALUATE
               END-IF
           END-PERFORM.
       REF-EXIT.
           EXIT.

       SAVE-CASSETTE SECTION.
       SAVE-START.
           REWRITE CASH-RECORD
               INVALID KEY
                   MOVE RC-IO-ERROR  TO CASH-OUT-RETCODE
                   MOVE EC-SYSTEM-IO TO CASH-OUT-ERROR-CODE
           END-REWRITE.
       SAVE-EXIT.
           EXIT.

       CLOSE-CASH SECTION.
       CLOSE-C-START.
           IF WS-OPENED = 'Y'
               CLOSE CASH-FILE
               MOVE 'N' TO WS-OPENED
           END-IF.
       CLOSE-C-EXIT.
           EXIT.

       END PROGRAM ATMCASH.
