      *****************************************************************
      * AUTHIF.cpy - ATMAUTH 呼出インタフェース
      *   CALL 'ATMAUTH' USING AUTH-PARM ATM-SESSION
      *   共通規約: 下位モジュールは STOP RUN せず GOBACK のみ。
      *****************************************************************
       01  AUTH-PARM.
           05  AUTH-FUNCTION           PIC X(08).
               88  AUTH-FN-VERIFY              VALUE 'VERIFY  '.
               88  AUTH-FN-CHANGE-PIN          VALUE 'CHGPIN  '.
      *        -- EJECT はカード排出 (ロック解放 + セッション破棄)。
      *        -- ファイルの CLOSE は端末停止時のみ。
               88  AUTH-FN-EJECT               VALUE 'EJECT   '.
               88  AUTH-FN-CLOSE               VALUE 'CLOSE   '.
      *        -- カードマスタを所有するのが ATMAUTH であるため、
      *        -- 出金限度額の判定と当日累計の更新もここに集約する。
               88  AUTH-FN-LIMIT-CHK           VALUE 'LIMITCHK'.
               88  AUTH-FN-ADD-DAILY           VALUE 'ADDDAILY'.
      *        -- PIN ハッシュ算出。式の所有者を ATMAUTH 1 箇所にする
      *        -- ための機能で、マスタ作成ユーティリティが利用する。
      *        -- HSM へ置き換える際もこの呼出だけが残る。
               88  AUTH-FN-HASH-PIN            VALUE 'HASHPIN '.
           05  AUTH-IN-PAN             PIC X(16).
           05  AUTH-IN-PIN             PIC X(04).
           05  AUTH-IN-NEW-PIN         PIC X(04).
           05  AUTH-IN-SALT            PIC 9(08).
           05  AUTH-IN-AMOUNT          PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  AUTH-OUT-RETCODE        PIC S9(04) COMP.
           05  AUTH-OUT-ERROR-CODE     PIC X(04).
           05  AUTH-OUT-HASH           PIC 9(12).
           05  AUTH-OUT-CARD-CAPTURED  PIC X(01).
