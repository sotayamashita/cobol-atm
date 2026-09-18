      *****************************************************************
      * PROGRAM : ATMPOST
      * PURPOSE : 取引記帳 (限度額判定・手数料算出・残高更新)
      * DESIGN  :
      *   検証順序は固定とする。理由は、利用者に返すエラーが端末ごとに
      *   ぶれないようにするため。この順序の所有者はこのモジュールで、
      *   下位モジュールは技術的な結果だけを返す。
      *     (1) 金額の形式妥当性     (2) 営業時間
      *     (3) 手数料算出           (4) カード限度額
      *     (5) 口座状態             (6) 支払可能残高
      *   残高は「元帳残高」と「支払可能残高」を同時に更新する。ATM 取引は
      *   即時反映のため両者は一致するが、将来の保留 (HOLD) 導入時に
      *   ここだけを変更すればよいよう、計算を分離している。
      *
      *   振替は 2 口座の更新を要する。単一ファイルの逐次更新では
      *   原子性を保証できないため、
      *     出金側を確定 → 入金側を確定 → 入金側失敗なら出金側を戻す
      *   という補償トランザクション方式をとる。補償自体が失敗した場合は
      *   RC-FATAL を返し、上位が EJ に不整合を記録して係員対応とする。
      *
      *   REVERSE は元記帳の逆仕訳。POST-IN-AMOUNT には元取引の金額を
      *   正の値で渡し、戻す向きは POST-IN-DIRECTION で指定する。
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMPOST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CONST.
           05  WS-MAX-TXN-AMOUNT       PIC S9(13)V99 VALUE 1000000.00.
      *    -- 24 時間稼働のため既定では全時間帯が営業時間。時間規制を
      *    -- 導入する際はこの 2 値を変える (外部マスタ化が望ましい)。
           05  WS-OPEN-HHMM            PIC 9(04) VALUE 0000.
           05  WS-CLOSE-HHMM           PIC 9(04) VALUE 2400.

      *    -- 出金手数料テーブル (時間帯別)。実運用では外部マスタ化する。
      *       開始時刻 HHMM + 手数料 (円, 9 桁)
       01  WS-FEE-TABLE.
           05  FILLER PIC X(13) VALUE '0000000000220'.
           05  FILLER PIC X(13) VALUE '0845000000000'.
           05  FILLER PIC X(13) VALUE '1800000000110'.
           05  FILLER PIC X(13) VALUE '2100000000220'.
       01  WS-FEE-ENTRIES REDEFINES WS-FEE-TABLE.
           05  WS-FEE-ENTRY OCCURS 4 TIMES.
               10  WS-FEE-FROM-HHMM    PIC 9(04).
               10  WS-FEE-AMT          PIC 9(09).

       01  WS-WORK.
           05  WS-I                    PIC S9(04) COMP VALUE ZERO.
           05  WS-HHMM                 PIC 9(04) VALUE ZERO.
           05  WS-TOTAL-DEBIT          PIC S9(13)V99 VALUE ZERO.
           05  WS-SPENDABLE            PIC S9(13)V99 VALUE ZERO.

      *    -- 自口座 / 相手口座のレコード像
       COPY 'ACCTREC.cpy'.
       COPY 'ACCTREC.cpy' REPLACING LEADING ==ACCT-== BY ==CPTY-==.

       COPY 'RETCODE.cpy'.

      *    -- 下位モジュール呼出用のパラメタ域
       COPY 'ACCTIF.cpy'.
       COPY 'AUTHIF.cpy'.

       LINKAGE SECTION.
       COPY 'POSTIF.cpy'.
       COPY 'ATMSESS.cpy'.

       PROCEDURE DIVISION USING POST-PARM ATM-SESSION.

       MAIN-CONTROL SECTION.
       MAIN-START.
           MOVE RC-OK   TO POST-OUT-RETCODE
           MOVE EC-NONE TO POST-OUT-ERROR-CODE
           MOVE ZERO    TO POST-OUT-FEE

           EVALUATE TRUE
               WHEN POST-FN-INQUIRY   PERFORM DO-INQUIRY
               WHEN POST-FN-WITHDRAW  PERFORM DO-WITHDRAW
               WHEN POST-FN-DEPOSIT   PERFORM DO-DEPOSIT
               WHEN POST-FN-TRANSFER  PERFORM DO-TRANSFER
               WHEN POST-FN-REVERSE   PERFORM DO-REVERSE
               WHEN POST-FN-CLOSE     PERFORM CLOSE-ACCT-FILE
               WHEN OTHER
                   MOVE RC-FATAL TO POST-OUT-RETCODE
           END-EVALUATE
           GOBACK.

      *================================================================
      * 残高照会
      *================================================================
       DO-INQUIRY SECTION.
       INQ-START.
           PERFORM READ-OWN-ACCOUNT
           IF POST-OUT-RETCODE NOT = RC-OK
               GO TO INQ-EXIT
           END-IF
           PERFORM CHECK-ACCT-STATUS
           IF POST-OUT-RETCODE NOT = RC-OK
               GO TO INQ-EXIT
           END-IF
           MOVE ACCT-LEDGER-BAL    TO POST-OUT-BAL-BEFORE
           MOVE ACCT-LEDGER-BAL    TO POST-OUT-BAL-AFTER
           MOVE ACCT-AVAILABLE-BAL TO POST-OUT-AVAILABLE.
       INQ-EXIT.
           EXIT.

      *================================================================
      * 出金
      *================================================================
       DO-WITHDRAW SECTION.
       WD-START.
           PERFORM VALIDATE-AMOUNT
           IF POST-OUT-RETCODE NOT = RC-OK
               GO TO WD-EXIT
           END-IF

           PERFORM CHECK-SERVICE-HOUR
           IF POST-OUT-RETCODE NOT = RC-OK
               GO TO WD-EXIT
           END-IF

           PERFORM CALC-FEE

           PERFORM CHECK-CARD-LIMIT
           IF POST-OUT-RETCODE NOT = RC-OK
               GO TO WD-EXIT
           END-IF

           PERFORM DEBIT-OWN-ACCOUNT
           IF POST-OUT-RETCODE NOT = RC-OK
               GO TO WD-EXIT
           END-IF

      *    -- 記帳確定後に当日累計へ反映する (手数料は限度額に含めない)
           PERFORM ADD-DAILY-USAGE.
       WD-EXIT.
           EXIT.

      *================================================================
      * 入金
      *================================================================
       DO-DEPOSIT SECTION.
       DP-START.
           PERFORM VALIDATE-AMOUNT
           IF POST-OUT-RETCODE NOT = RC-OK
               GO TO DP-EXIT
           END-IF

           PERFORM LOCK-OWN-ACCOUNT
           IF POST-OUT-RETCODE NOT = RC-OK
               GO TO DP-EXIT
           END-IF
           PERFORM CHECK-ACCT-STATUS
           IF POST-OUT-RETCODE NOT = RC-OK
               PERFORM UNLOCK-ACCOUNT
               GO TO DP-EXIT
           END-IF

           MOVE ACCT-LEDGER-BAL TO POST-OUT-BAL-BEFORE
           PERFORM CREDIT-OWN-BALANCE
           PERFORM UPDATE-OWN-ACCOUNT
           IF POST-OUT-RETCODE NOT = RC-OK
               GO TO DP-EXIT
           END-IF
           PERFORM PUBLISH-OWN-BALANCE.
       DP-EXIT.
           EXIT.

      *================================================================
      * 振替 (出金側確定 → 入金側確定 → 失敗時は出金側を補償)
      *================================================================
       DO-TRANSFER SECTION.
       TR-START.
           PERFORM VALIDATE-AMOUNT
           IF POST-OUT-RETCODE NOT = RC-OK
               GO TO TR-EXIT
           END-IF

           IF POST-IN-CPTY-ACCT-NO = SESS-ACCT-NO
               MOVE RC-BUSINESS-ERROR TO POST-OUT-RETCODE
               MOVE EC-AMOUNT-INVALID TO POST-OUT-ERROR-CODE
               GO TO TR-EXIT
           END-IF

           PERFORM CALC-FEE

      *    -- 相手口座の存在と受入可否を先に確認する。出金だけ成立して
      *    -- 相手が存在しない、という最悪ケースを構造的に避ける。
           PERFORM CHECK-COUNTERPARTY
           IF POST-OUT-RETCODE NOT = RC-OK
               GO TO TR-EXIT
           END-IF

           PERFORM DEBIT-OWN-ACCOUNT
           IF POST-OUT-RETCODE NOT = RC-OK
               GO TO TR-EXIT
           END-IF

           PERFORM CREDIT-COUNTERPARTY
           IF POST-OUT-RETCODE NOT = RC-OK
               PERFORM COMPENSATE-OWN-ACCOUNT
           END-IF.
       TR-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 相手口座の事前確認。状態判定もここで行う (検証順序 5 に相当)。
      *----------------------------------------------------------------
       CHECK-COUNTERPARTY SECTION.
       CCP-START.
           SET ACCT-FN-READ TO TRUE
           MOVE POST-IN-CPTY-ACCT-NO TO ACCT-IN-ACCT-NO
           CALL 'ATMACCT' USING ACCT-PARM ATM-SESSION
           PERFORM PROPAGATE-ACCT-ERROR
           IF POST-OUT-RETCODE NOT = RC-OK
               GO TO CCP-EXIT
           END-IF

           MOVE ACCT-IO-RECORD TO CPTY-RECORD
           EVALUATE TRUE
               WHEN CPTY-ST-CLOSED
                   MOVE RC-BUSINESS-ERROR TO POST-OUT-RETCODE
                   MOVE EC-ACCT-CLOSED    TO POST-OUT-ERROR-CODE
               WHEN CPTY-ST-FROZEN
                   MOVE RC-BUSINESS-ERROR TO POST-OUT-RETCODE
                   MOVE EC-ACCT-FROZEN    TO POST-OUT-ERROR-CODE
               WHEN CPTY-ST-DORMANT
                   MOVE RC-BUSINESS-ERROR TO POST-OUT-RETCODE
                   MOVE EC-ACCT-DORMANT   TO POST-OUT-ERROR-CODE
           END-EVALUATE.
       CCP-EXIT.
           EXIT.

       CREDIT-COUNTERPARTY SECTION.
       CP-START.
           SET ACCT-FN-LOCK TO TRUE
           MOVE POST-IN-CPTY-ACCT-NO TO ACCT-IN-ACCT-NO
           CALL 'ATMACCT' USING ACCT-PARM ATM-SESSION
           PERFORM PROPAGATE-ACCT-ERROR
           IF POST-OUT-RETCODE NOT = RC-OK
               GO TO CP-EXIT
           END-IF

           MOVE ACCT-IO-RECORD TO CPTY-RECORD
           ADD POST-IN-AMOUNT TO CPTY-LEDGER-BAL
           ADD POST-IN-AMOUNT TO CPTY-AVAILABLE-BAL
           MOVE CPTY-RECORD TO ACCT-IO-RECORD

           SET ACCT-FN-UPDATE TO TRUE
           CALL 'ATMACCT' USING ACCT-PARM ATM-SESSION
           PERFORM PROPAGATE-ACCT-ERROR.
       CP-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 補償: 出金済の自口座へ全額 (手数料含む) を戻す
      *----------------------------------------------------------------
       COMPENSATE-OWN-ACCOUNT SECTION.
       COMP-START.
           PERFORM LOCK-OWN-ACCOUNT
           IF POST-OUT-RETCODE NOT = RC-OK
               MOVE RC-FATAL TO POST-OUT-RETCODE
               GO TO COMP-EXIT
           END-IF

           ADD WS-TOTAL-DEBIT TO ACCT-LEDGER-BAL
           ADD WS-TOTAL-DEBIT TO ACCT-AVAILABLE-BAL
           PERFORM UPDATE-OWN-ACCOUNT
           IF POST-OUT-RETCODE NOT = RC-OK
      *        -- 補償失敗。自動復旧不能。係員対応が必須。
               MOVE RC-FATAL TO POST-OUT-RETCODE
           ELSE
               PERFORM PUBLISH-OWN-BALANCE
           END-IF.
       COMP-EXIT.
           EXIT.

      *================================================================
      * 取消 (紙幣繰出失敗などで記帳を巻き戻す)
      *   POST-IN-AMOUNT は元取引の金額 (正)。
      *   POST-IN-DIRECTION が 'C' なら口座へ戻し、'D' なら引き直す。
      *================================================================
       DO-REVERSE SECTION.
       RV-START.
           PERFORM LOCK-OWN-ACCOUNT
           IF POST-OUT-RETCODE NOT = RC-OK
               GO TO RV-EXIT
           END-IF

           MOVE ACCT-LEDGER-BAL TO POST-OUT-BAL-BEFORE
           COMPUTE WS-TOTAL-DEBIT = POST-IN-AMOUNT + SESS-TXN-FEE
           IF POST-DIR-CREDIT
               ADD WS-TOTAL-DEBIT TO ACCT-LEDGER-BAL
               ADD WS-TOTAL-DEBIT TO ACCT-AVAILABLE-BAL
           ELSE
               SUBTRACT WS-TOTAL-DEBIT FROM ACCT-LEDGER-BAL
               SUBTRACT WS-TOTAL-DEBIT FROM ACCT-AVAILABLE-BAL
           END-IF

           PERFORM UPDATE-OWN-ACCOUNT
           IF POST-OUT-RETCODE NOT = RC-OK
               MOVE RC-FATAL TO POST-OUT-RETCODE
               GO TO RV-EXIT
           END-IF
           PERFORM PUBLISH-OWN-BALANCE

      *    -- 当日累計に載るのは出金のみ。戻す向きが貸方 (口座へ戻す)
      *    -- のときだけ累計から差し引く。
           IF POST-DIR-CREDIT
               SET AUTH-FN-ADD-DAILY TO TRUE
               COMPUTE AUTH-IN-AMOUNT = 0 - POST-IN-AMOUNT
               CALL 'ATMAUTH' USING AUTH-PARM ATM-SESSION
           END-IF.
       RV-EXIT.
           EXIT.

      *================================================================
      * 記帳プリミティブ
      *================================================================

      *----------------------------------------------------------------
      * 自口座からの引き落とし。出金と振替の共通処理。
      * 口座状態 → 支払可能残高 の順で判定し、成立したら更新する。
      *----------------------------------------------------------------
       DEBIT-OWN-ACCOUNT SECTION.
       DOA-START.
           COMPUTE WS-TOTAL-DEBIT = POST-IN-AMOUNT + POST-OUT-FEE

           PERFORM LOCK-OWN-ACCOUNT
           IF POST-OUT-RETCODE NOT = RC-OK
               GO TO DOA-EXIT
           END-IF

           PERFORM CHECK-ACCT-STATUS
           IF POST-OUT-RETCODE NOT = RC-OK
               PERFORM UNLOCK-ACCOUNT
               GO TO DOA-EXIT
           END-IF

      *    -- 当座貸越枠まで引き出せる
           COMPUTE WS-SPENDABLE =
               ACCT-AVAILABLE-BAL + ACCT-OVERDRAFT-LIMIT
           IF WS-TOTAL-DEBIT > WS-SPENDABLE
               MOVE RC-BUSINESS-ERROR     TO POST-OUT-RETCODE
               MOVE EC-INSUFFICIENT-FUNDS TO POST-OUT-ERROR-CODE
               PERFORM UNLOCK-ACCOUNT
               GO TO DOA-EXIT
           END-IF

           MOVE ACCT-LEDGER-BAL TO POST-OUT-BAL-BEFORE
           SUBTRACT WS-TOTAL-DEBIT FROM ACCT-LEDGER-BAL
           SUBTRACT WS-TOTAL-DEBIT FROM ACCT-AVAILABLE-BAL
           PERFORM UPDATE-OWN-ACCOUNT
           IF POST-OUT-RETCODE NOT = RC-OK
               GO TO DOA-EXIT
           END-IF
           PERFORM PUBLISH-OWN-BALANCE.
       DOA-EXIT.
           EXIT.

       CREDIT-OWN-BALANCE SECTION.
       COB-START.
           ADD POST-IN-AMOUNT TO ACCT-LEDGER-BAL
           ADD POST-IN-AMOUNT TO ACCT-AVAILABLE-BAL.
       COB-EXIT.
           EXIT.

       PUBLISH-OWN-BALANCE SECTION.
       POB-START.
           MOVE ACCT-LEDGER-BAL    TO POST-OUT-BAL-AFTER
           MOVE ACCT-AVAILABLE-BAL TO POST-OUT-AVAILABLE.
       POB-EXIT.
           EXIT.

      *================================================================
      * 検証部品
      *================================================================
       VALIDATE-AMOUNT SECTION.
       VAL-START.
           IF POST-IN-AMOUNT <= ZERO
               MOVE RC-BUSINESS-ERROR TO POST-OUT-RETCODE
               MOVE EC-AMOUNT-INVALID TO POST-OUT-ERROR-CODE
               GO TO VAL-EXIT
           END-IF
           IF POST-IN-AMOUNT > WS-MAX-TXN-AMOUNT
               MOVE RC-BUSINESS-ERROR TO POST-OUT-RETCODE
               MOVE EC-LIMIT-PER-TXN  TO POST-OUT-ERROR-CODE
           END-IF.
       VAL-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 口座状態の判定。閉鎖・凍結・休眠は以降の取引を一切許さない。
      * 検証順序 5 に相当し、この順序の所有者は ATMPOST である。
      *----------------------------------------------------------------
       CHECK-ACCT-STATUS SECTION.
       CAS-START.
           EVALUATE TRUE
               WHEN ACCT-ST-CLOSED
                   MOVE RC-BUSINESS-ERROR TO POST-OUT-RETCODE
                   MOVE EC-ACCT-CLOSED    TO POST-OUT-ERROR-CODE
               WHEN ACCT-ST-FROZEN
                   MOVE RC-BUSINESS-ERROR TO POST-OUT-RETCODE
                   MOVE EC-ACCT-FROZEN    TO POST-OUT-ERROR-CODE
               WHEN ACCT-ST-DORMANT
                   MOVE RC-BUSINESS-ERROR TO POST-OUT-RETCODE
                   MOVE EC-ACCT-DORMANT   TO POST-OUT-ERROR-CODE
           END-EVALUATE.
       CAS-EXIT.
           EXIT.

       CHECK-SERVICE-HOUR SECTION.
       SVC-START.
           PERFORM EXTRACT-HHMM
           IF WS-HHMM < WS-OPEN-HHMM OR WS-HHMM >= WS-CLOSE-HHMM
               MOVE RC-BUSINESS-ERROR      TO POST-OUT-RETCODE
               MOVE EC-OUT-OF-SERVICE-HOUR TO POST-OUT-ERROR-CODE
           END-IF.
       SVC-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 手数料: テーブルを上から走査し、該当する最後の区分を採用する
      *----------------------------------------------------------------
       CALC-FEE SECTION.
       FEE-START.
           MOVE ZERO TO POST-OUT-FEE
           PERFORM EXTRACT-HHMM
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               IF WS-HHMM >= WS-FEE-FROM-HHMM(WS-I)
                   MOVE WS-FEE-AMT(WS-I) TO POST-OUT-FEE
               END-IF
           END-PERFORM
           MOVE POST-OUT-FEE TO SESS-TXN-FEE.
       FEE-EXIT.
           EXIT.

       EXTRACT-HHMM SECTION.
       EXH-START.
           COMPUTE WS-HHMM =
               FUNCTION MOD (SESS-TIMESTAMP / 100, 10000).
       EXH-EXIT.
           EXIT.

       CHECK-CARD-LIMIT SECTION.
       CLM-START.
           SET AUTH-FN-LIMIT-CHK TO TRUE
           MOVE POST-IN-AMOUNT   TO AUTH-IN-AMOUNT
           CALL 'ATMAUTH' USING AUTH-PARM ATM-SESSION
           IF AUTH-OUT-RETCODE NOT = RC-OK
               MOVE AUTH-OUT-RETCODE    TO POST-OUT-RETCODE
               MOVE AUTH-OUT-ERROR-CODE TO POST-OUT-ERROR-CODE
           END-IF.
       CLM-EXIT.
           EXIT.

       ADD-DAILY-USAGE SECTION.
       ADU-START.
           SET AUTH-FN-ADD-DAILY TO TRUE
           MOVE POST-IN-AMOUNT   TO AUTH-IN-AMOUNT
           CALL 'ATMAUTH' USING AUTH-PARM ATM-SESSION.
       ADU-EXIT.
           EXIT.

      *================================================================
      * 口座 I/O ラッパ
      *================================================================
       READ-OWN-ACCOUNT SECTION.
       ROA-START.
           SET ACCT-FN-READ  TO TRUE
           MOVE SESS-ACCT-NO TO ACCT-IN-ACCT-NO
           CALL 'ATMACCT' USING ACCT-PARM ATM-SESSION
           MOVE ACCT-IO-RECORD TO ACCT-RECORD
           PERFORM PROPAGATE-ACCT-ERROR.
       ROA-EXIT.
           EXIT.

       LOCK-OWN-ACCOUNT SECTION.
       LOA-START.
           SET ACCT-FN-LOCK  TO TRUE
           MOVE SESS-ACCT-NO TO ACCT-IN-ACCT-NO
           CALL 'ATMACCT' USING ACCT-PARM ATM-SESSION
           MOVE ACCT-IO-RECORD TO ACCT-RECORD
           PERFORM PROPAGATE-ACCT-ERROR.
       LOA-EXIT.
           EXIT.

       UPDATE-OWN-ACCOUNT SECTION.
       UOA-START.
           MOVE ACCT-RECORD   TO ACCT-IO-RECORD
           SET ACCT-FN-UPDATE TO TRUE
           CALL 'ATMACCT' USING ACCT-PARM ATM-SESSION
           PERFORM PROPAGATE-ACCT-ERROR
           IF POST-OUT-RETCODE = RC-OK
               MOVE ACCT-IO-RECORD TO ACCT-RECORD
           END-IF.
       UOA-EXIT.
           EXIT.

      *    -- 取引単位のロック解放。ファイルは閉じない。
       UNLOCK-ACCOUNT SECTION.
       ULA-START.
           SET ACCT-FN-UNLOCK TO TRUE
           CALL 'ATMACCT' USING ACCT-PARM ATM-SESSION.
       ULA-EXIT.
           EXIT.

      *    -- 端末停止時のみ呼ばれる
       CLOSE-ACCT-FILE SECTION.
       CAF-START.
           SET ACCT-FN-CLOSE TO TRUE
           CALL 'ATMACCT' USING ACCT-PARM ATM-SESSION.
       CAF-EXIT.
           EXIT.

       PROPAGATE-ACCT-ERROR SECTION.
       PAE-START.
           IF ACCT-OUT-RETCODE NOT = RC-OK
               MOVE ACCT-OUT-RETCODE    TO POST-OUT-RETCODE
               MOVE ACCT-OUT-ERROR-CODE TO POST-OUT-ERROR-CODE
           END-IF.
       PAE-EXIT.
           EXIT.

       END PROGRAM ATMPOST.
