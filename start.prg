SET TALK OFF

#DEFINE DEBUG_RUN .f.
#DEFINE ONLY_STRUCTURE .t.

SET CENTURY ON 
SET CURRENCY TO 

#define UV "'"
#define ZPUV "`"
#define CRLF CHR(13)+CHR(10)
#define IND "   "

LOCAL lcDbc 
lcDbc = GETFILE('dbc')
IF !empty(lcDbc )
	= build_database(lcDbc )
ENDIF

SET TALK ON
RETURN

PROCEDURE build_database
&& =====================
	PARAMETERS pcDBC
	LOCAL i,liPocet ,lcSql
	PUBLIC liHA
	CLOSE DATABASES all
	lcSql = ADDBS(JUSTPATH(SYS(16,0)))+'sql_dump_'+JUSTSTEM(pcDBC)+sys[2015]+'.sql'
	liHa = FCREATE(lcSql )
	OPEN DATABASE (pcDBC) EXCLUSIVE 
	liPocet = ADBOBJECTS(atable,"TABLE")
	= ASORT(atable)
	FOR i = 1 TO liPocet 
		= build_table(atable[i])
		= FWRITE(liHa,CRLF)
	ENDFOR 
	FOR i = 1 TO liPocet 
		= build_constraint(atable[i])
	ENDFOR 
	= FCLOSE(liHa)
	RELEASE liHA
	CLOSE DATABASES all
ENDPROC

PROCEDURE build_constraint
&& =======================
	PARAMETERS pcTab
	LOCAL lcTabString 
	lcTabString = create_foreignkeys(pcTab)
	IF !EMPTY(lcTabString )
		= FWRITE(liHa,lcTabString )
	ENDIF 
ENDPROC 

PROCEDURE build_table
&& ==================
	PARAMETERS pcTab
	LOCAL i,liPocet ,loZaznam,lcRadek , lcTabString 
	USE (pcTab) IN 0 EXCLUSIVE 
	SELECT (pcTab)
	liPocet = AFIELDS(aPole)
	liCodePage = CPDBF()
&&
&&
&&
*!*	1 Field name - Character
*!*	2 Field type:
*!*	C = Character										OK
*!*	Y = Currency										OK
*!*	D = Date											OK
*!*	T = DateTime										OK
*!*	B = Double											OK
*!*	F = Float											OK
*!*	G = General											Is not possible
*!*	I = Integer											OK
*!*	L = Logical											OK
*!*	M = Memo											OK
*!*	N = Numeric											OK
*!*	Q = Varbinary
*!*	V = Varchar and Varchar (Binary)					OK
*!*	W = Blob
*!*	3 Field width
*!*	4 Decimal places
*!*	5 Null values allowed - Logical						OK
*!*	6 Code page translation not allowed - Logical
*!*	7 Field validation expression - Character
*!*	8 Field validation text - Character
*!*	9 Field default value - Character
*!*	10 Table validation expression - Character
*!*	11 Table validation text - Character
*!*	12 Long table name - Character
*!*	13 Insert trigger expression - Character
*!*	14 Update trigger expression - Character
*!*	15 Delete trigger expression - Character
*!*	16 Table comment - Character						OK
*!*	17 NextValue for autoincrementing - Numeric
*!*	18 Step for autoincrementing - Numeric
&&
&&
&&
	lcTabString = create_table(pcTab, liCodePage , @aPole)
	= FWRITE(liHa,lcTabString )
	IF !ONLY_STRUCTURE 
		DO WHILE !EOF()
			SCATTER NAME loZaznam MEMO 
			lcRadek = insert_record(pcTab,@aPole,loZaznam)
			= FWRITE(liHa,lcRadek )
			SKIP +1
			IF DEBUG_RUN 
				IF RECNO() > 2
					EXIT 
				ENDIF 
			ENDIF 
		ENDDO 
	ENDIF 
	USE IN (pcTab)
ENDPROC

PROCEDURE create_table
&& ===================
	PARAMETERS pcTabName, piCodePage , paPole
	LOCAL liPocet , i, lcStr, lcNULL , lcDefault , lcComment , lcCmnt , lcCodePage, lcStrForeignKeys , lcStrIndexes 
	liPocet = ALEN(paPole,1)
	lcStr = 'DROP TABLE IF EXISTS ' + ZPUV + pcTabName + ZPUV + ';' + CRLF 
	lcStr = lcStr + 'CREATE TABLE ' + ZPUV + pcTabName + ZPUV + ' ( ' + CRLF 
	lcTCmnt = DBGetProp(pcTabName, "Table", "Comment")
	lcTComment = IIF(!EMPTY(lcTCmnt ), " COMMENT '" + dej_string(lcTCmnt) + "' ", '')
	lcCodePage =  ' CHARSET=cp' + ALLTRIM(TRANSFORM(piCodePage )) + ' '
	FOR i = 1 to liPocet
		lcNULL = IIF(paPole[i,5], ' NULL ', ' NOT NULL ')
		lcCmnt = DBGetProp(pcTabName + '.' + paPole[i,1], "Field", "Comment")
		lcComment = IIF(!EMPTY(lcCmnt ), " COMMENT '" + dej_string(lcCmnt) + "' ", '')
		DO CASE 
		CASE paPole[i,2] = 'I'
			lcDefault = IIF(INLIST(LEFT(paPole[i,9],1),['],["]) .and. INLIST(RIGHT(paPole[i,9],1),['],["]), ;
				SUBSTR(paPole[i,9], 2, LEN(paPole[i,9]) - 2), paPole[i,9])
			lcDefault = IIF(!EMPTY(lcDefault ), ' DEFAULT ' + '"' + lcDefault + '"', ' DEFAULT 0 ')
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + " int(11) " + lcNULL + lcDefault + lcComment + "," + CRLF 
		CASE paPole[i,2] = 'C'
			lcDefault = IIF(INLIST(LEFT(paPole[i,9],1),['],["]) .and. INLIST(RIGHT(paPole[i,9],1),['],["]), ;
				SUBSTR(paPole[i,9], 2, LEN(paPole[i,9]) - 2), paPole[i,9])
			lcDefault = IIF(!EMPTY(lcDefault ), ' DEFAULT ' + '"' + lcDefault  + '"', ' DEFAULT "" ')
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + " char(" + TRANSFORM(paPole[i,3]) + ") " + lcNULL + lcDefault + lcComment + "," + CRLF 
		CASE paPole[i,2] = 'V'
			lcDefault = IIF(INLIST(LEFT(paPole[i,9],1),['],["]) .and. INLIST(RIGHT(paPole[i,9],1),['],["]), ;
				SUBSTR(paPole[i,9], 2, LEN(paPole[i,9]) - 2), paPole[i,9])
			lcDefault = IIF(!EMPTY(lcDefault ), ' DEFAULT ' + '"' + lcDefault  + '"', ' DEFAULT "" ')
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + " varchar(" + TRANSFORM(paPole[i,3]) + ") " + lcNULL + lcDefault + lcComment + "," + CRLF 
		CASE paPole[i,2] = 'N'
			lcDefault = IIF(INLIST(LEFT(paPole[i,9],1),['],["]) .and. INLIST(RIGHT(paPole[i,9],1),['],["]), ;
				SUBSTR(paPole[i,9], 2, LEN(paPole[i,9]) - 2), paPole[i,9])
			lcDefault = IIF(!EMPTY(lcDefault ), ' DEFAULT ' + '"' + lcDefault  + '"', ' DEFAULT 0 ')
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + ;
				" decimal(" + TRANSFORM(paPole[i,3] + IIF(paPole[i,4] > 0,paPole[i,4] + 1,0)) + ;
				"," + TRANSFORM(paPole[i,4]) + ") " + lcNULL + lcDefault + lcComment + "," + CRLF
		CASE paPole[i,2] = 'D'
			lcDefault = IIF(INLIST(LEFT(paPole[i,9],1),['],["]) .and. INLIST(RIGHT(paPole[i,9],1),['],["]), ;
				SUBSTR(paPole[i,9], 2, LEN(paPole[i,9]) - 2), paPole[i,9])
			lcDefault = IIF(!EMPTY(lcDefault ), ' DEFAULT ' + '"' + lcDefault  + '"', ' DEFAULT "0000-00-00" ')
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + " date " + lcNULL + lcDefault + lcComment + "," + CRLF 
		CASE paPole[i,2] = 'T'
			lcDefault = IIF(INLIST(LEFT(paPole[i,9],1),['],["]) .and. INLIST(RIGHT(paPole[i,9],1),['],["]), ;
				SUBSTR(paPole[i,9], 2, LEN(paPole[i,9]) - 2), paPole[i,9])
			lcDefault = IIF(!EMPTY(lcDefault ), ' DEFAULT ' + '"' + lcDefault  + '"', ' DEFAULT "0000-00-00 00:00:00" ')
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + " datetime " + lcNULL + lcDefault + lcComment + "," + CRLF 
		CASE paPole[i,2] = 'L'
			lcDefault = IIF(INLIST(LEFT(paPole[i,9],1),['],["]) .and. INLIST(RIGHT(paPole[i,9],1),['],["]), ;
				SUBSTR(paPole[i,9], 2, LEN(paPole[i,9]) - 2), paPole[i,9])
			lcDefault = IIF(!EMPTY(lcDefault ), ' DEFAULT TRUE ', ' DEFAULT FALSE ')
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + " boolean " + lcNULL + lcDefault + lcComment + "," + CRLF 
		CASE paPole[i,2] = 'B'
			lcDefault = IIF(INLIST(LEFT(paPole[i,9],1),['],["]) .and. INLIST(RIGHT(paPole[i,9],1),['],["]), ;
				SUBSTR(paPole[i,9], 2, LEN(paPole[i,9]) - 2), paPole[i,9])
			lcDefault = IIF(!EMPTY(lcDefault ), ' DEFAULT ' + '"' + lcDefault  + '"', ' DEFAULT 0 ')
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + " double " + lcNULL + lcDefault + lcComment + "," + CRLF 
		CASE paPole[i,2] = 'F'
			lcDefault = IIF(INLIST(LEFT(paPole[i,9],1),['],["]) .and. INLIST(RIGHT(paPole[i,9],1),['],["]), ;
				SUBSTR(paPole[i,9], 2, LEN(paPole[i,9]) - 2), paPole[i,9])
			lcDefault = IIF(!EMPTY(lcDefault ), ' DEFAULT ' + '"' + lcDefault  + '"', ' DEFAULT 0 ')
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + " float " + lcNULL + lcDefault + lcComment + "," + CRLF 
		CASE paPole[i,2] = 'Y'
			lcDefault = IIF(INLIST(LEFT(paPole[i,9],1),['],["]) .and. INLIST(RIGHT(paPole[i,9],1),['],["]), ;
				SUBSTR(paPole[i,9], 2, LEN(paPole[i,9]) - 2), paPole[i,9])
			lcDefault = IIF(!EMPTY(lcDefault ), ' DEFAULT ' + '"' + lcDefault  + '"', ' DEFAULT 0 ')
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + " decimal(20,4) " + lcNULL + lcDefault + lcComment + "," + CRLF 
		CASE paPole[i,2] = 'M'
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + " text " + " NULL " + lcComment + "," + CRLF 
		ENDCASE 
	ENDFOR 
	lcStrIndexes = create_indexes(pcTabName)
	IF !EMPTY(lcStrIndexes )
		lcStr = lcStr + lcStrIndexes 
	ENDIF 
*!*		lcStrForeignKeys = create_foreignkeys(pcTabName)
*!*		IF !EMPTY(lcStrForeignKeys )
*!*			lcStr = lcStr + lcStrForeignKeys 
*!*		ENDIF 
	lcStr = LEFT(lcStr,LEN(lcStr)-3) + CRLF 		&& strip ",CRLF"
	lcStr = lcStr + IND + ') ' + lcTComment + ' ENGINE=INNODB DEFAULT ' + lcCodePage + ' ROW_FORMAT=DEFAULT;' + CRLF
	lcStr = lcStr + CRLF
	RETURN lcStr 
ENDPROC

PROCEDURE create_indexes
&& =====================
	PARAMETERS pcTabName
	LOCAL nCount , lcTagName , lcTagExpr , lcExpr , lcTag, lcCommands, lcIndexType , liIndexCount 
	lcCommands = ''
	liIndexCount = 0
	FOR nCount = 1 TO TAGCOUNT( )
	   IF !EMPTY(TAG(nCount))  			&& Checks for tags in the index
	   		lcTagName = TAG(nCount)     && Display structural index names
	   		lcTagExpr = KEY(nCount)
	   		lcExpr = ''
	   		IF !('('$lcTagExpr .or. '+'$lcTagExpr .or. '!'$lcTagExpr .or. '-'$lcTagExpr)
	   			lcExpr = LOWER(lcTagExpr )
	   			lcIndexType = IIF(PRIMARY(nCount), 'PRIMARY KEY', 'INDEX')
	   			lcTag = IIF(PRIMARY(nCount), 'pk_' + lcExpr, 'idx_' + lcExpr )
	   			IF !lcTag$lcCommands 
	   				lcCommands = lcCommands + ;
	   					IND + lcIndexType + " " + ZPUV + lcTag + ZPUV + " (" + ZPUV + lcExpr + ZPUV + ")" + ',' + CRLF
			   		liIndexCount = liIndexCount + 1
	   			ENDIF 
	   		ENDIF 
	   ENDIF
	ENDFOR
	RETURN lcCommands 
ENDPROC 

PROCEDURE create_foreignkeys
&& =========================
	PARAMETERS pcTabName
	LOCAL lcRetStr, liCnt, liFKCount
	lcRetStr = ''
	liFKCount = 0
	liCnt = ADBOBJECTS(laRelations,"RELATION")
	FOR i = 1 TO liCnt 
		IF laRelations[i,1] == pcTabName
			lcChildTable = laRelations[i,1]
			lcParentTable = laRelations[i,2]
			lcChildKey = laRelations[i,3]
			lcParentKey = laRelations[i,4]
			lcRefIntInfo = RTRIM(laRelations[i,5])
			lcRetStr = lcRetStr + ;
				'FOREIGN KEY (`' + lcChildKey + '`) REFERENCES `' + lcParentTable + '` (`' + lcParentKey + '`)' + ',' + CRLF
			liFKCount = liFKCount + 1
		ENDIF 
	ENDFOR 
	IF liFKCount > 0
		lcRetStr = 'ALTER TABLE ' + ZPUV + (pcTabName) + ZPUV + ' ' + CRLF + IND + "ADD " + lcRetStr 
		lcRetStr = LEFT(lcRetStr ,LEN(lcRetStr )-3) + ';' + CRLF 		&& strip ",CRLF"
	ENDIF 
	RETURN lcRetStr
ENDPROC 

PROCEDURE insert_record
&& ====================
	PARAMETERS pcTab,paPole,poZaznam
	LOCAL liPocet ,i,lcSeznamPoli,lcSeznamHodnot,lcRadek 
	STORE '' TO lcSeznamPoli,lcSeznamHodnot
	liPocet = ALEN(paPole,1)
	FOR i = 1 to liPocet
		DO CASE 
		CASE INLIST(paPole[i,2],'I','C','V','N','D','T','L','B','F','Y','M')
			= build_fields(paPole[i,1],paPole[i,2],EVALUATE('pozaznam.'+paPole[i,1]),@lcSeznamPoli,@lcSeznamHodnot)
		OTHERWISE 
			&& do nothing
		ENDCASE 
	ENDFOR 
	lcRadek = 'INSERT INTO '+ ZPUV + (pcTab) + ZPUV + ' ( ' + lcSeznamPoli + ' )' + ' VALUES ( ' + lcSeznamHodnot + ' ) ;' + CRLF
	RETURN lcRadek 
ENDPROC 

PROCEDURE build_fields
&& ===================
	PARAMETERS pcJmeno,pcTyp,peValue,pcSeznamPoli,pcSeznamHodnot

	pcSeznamPoli = pcSeznamPoli + IIF(EMPTY(pcSeznamPoli), ZPUV + pcJmeno + ZPUV, ', ' + ZPUV + pcJmeno + ZPUV)
	DO CASE 
	CASE pcTyp = 'I'
		pcValue = dej_cislo(peValue )
	CASE pcTyp = 'C'
		pcValue = UV + dej_string(RTRIM(peValue)) + UV
	CASE pcTyp = 'V'
		pcValue = UV + dej_string(RTRIM(peValue)) + UV
	CASE pcTyp = 'N'
		pcValue = dej_cislo(peValue )
	CASE pcTyp = 'D'
		pcValue = UV + TRANSFORM(year(peValue)) + '-' + TRANSFORM(MONTH(peValue)) + '-' + TRANSFORM(DAY(peValue)) + UV
	CASE pcTyp = 'T'
		pcValue = UV + TRANSFORM(year(peValue)) + '-' + TRANSFORM(MONTH(peValue)) + '-' + TRANSFORM(DAY(peValue)) + ' ' + ;
			TRANSFORM(HOUR(peValue)) + ':' + TRANSFORM(MINUTE(peValue)) + ':' + TRANSFORM(SEC(peValue)) + UV
	CASE pcTyp = 'L'
		pcValue = iif(peValue,'0b1','0b0')
	CASE pcTyp = 'B'
		pcValue = dej_cislo(peValue )
	CASE pcTyp = 'F'
		pcValue = dej_cislo(peValue )
	CASE pcTyp = 'Y'
		pcValue = dej_cislo(peValue )
		pcValue = RIGHT(pcValue,LEN(pcValue)-1)
	CASE pcTyp = 'M'
		pcValue = UV + dej_string(RTRIM(peValue)) + UV
	ENDCASE
	pcSeznamHodnot = pcSeznamHodnot + IIF(EMPTY(pcSeznamHodnot),pcValue,', '+pcValue)
ENDPROC

PROCEDURE dej_string
&& =================
	PARAMETERS pcString
	LOCAL lcString
	lcString = STRTRAN(pcString,"\","\\")
	lcString = STRTRAN(lcString ,"'","\'")
	lcString = STRTRAN(lcString ,'"','\"')
	lcString = STRTRAN(lcString ,CHR(13),"\r")
	lcString = STRTRAN(lcString ,CHR(10),"\n")
	lcString = STRTRAN(lcString ,CHR(0),"\0")
	RETURN lcString
ENDPROC 

PROCEDURE dej_cislo
&& ================
	PARAMETERS pnNumber
	LOCAL lcString
	lcString = ALLTRIM(TRANSFORM(pnNumber))
	lcString = IIF('*'$lcString , '0', lcString )
	RETURN lcString
ENDPROC 
