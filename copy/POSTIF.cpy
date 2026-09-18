      *****************************************************************
      * POSTIF.cpy - ATMPOST 呼出インタフェース
      *   CALL 'ATMPOST' USING POST-PARM ATM-SESSION
      *   REVERSE は SESS-TXN-FEE に元取引の手数料が残っている前提。
      *   金額は常に正の値で渡し、向きは POST-IN-DIRECTION で示す。
      *   呼出元ごとに符号の解釈が変わらないようにするため。
      *****************************************************************
       01  POST-PARM.
           05  POST-FUNCTION           PIC X(08).
               88  POST-FN-INQUIRY             VALUE 'INQUIRY '.
               88  POST-FN-WITHDRAW            VALUE 'WITHDRAW'.
               88  POST-FN-DEPOSIT             VALUE 'DEPOSIT '.
               88  POST-FN-TRANSFER            VALUE 'TRANSFER'.
               88  POST-FN-REVERSE             VALUE 'REVERSE '.
      *        -- 端末停止時に口座マスタを解放する
               88  POST-FN-CLOSE               VALUE 'CLOSE   '.
           05  POST-IN-AMOUNT          PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  POST-IN-CPTY-ACCT-NO    PIC X(10).
      *    -- REVERSE の戻し方向。C = 口座へ戻す (出金の取消)、
      *    --                      D = 口座から引く (入金の取消)
           05  POST-IN-DIRECTION       PIC X(01).
               88  POST-DIR-CREDIT             VALUE 'C'.
               88  POST-DIR-DEBIT              VALUE 'D'.
           05  POST-OUT-RETCODE        PIC S9(04) COMP.
           05  POST-OUT-ERROR-CODE     PIC X(04).
           05  POST-OUT-FEE            PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  POST-OUT-BAL-BEFORE     PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  POST-OUT-BAL-AFTER      PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  POST-OUT-AVAILABLE      PIC S9(13)V99 SIGN LEADING SEPARATE.
      *    -- 他行あて振込のときだけ設定される。入金日が当日でない
      *    -- 場合があるため、利用者に知らせる必要がある。
           05  POST-OUT-CPTY-BANK-NAME PIC X(30).
           05  POST-OUT-VALUE-DATE     PIC 9(08).
