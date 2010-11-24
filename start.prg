SET TALK OFF

&&
&& INIT
&&

SET CENTURY ON 
SET CURRENCY TO 

#define UV "'"
#define ZPUV "`"
#define CRLF CHR(13)+CHR(10)
#define IND "   "

LOCAL lcDbc 
lcDbc = GETFILE('dbc')
IF !empty(lcDbc )
	= zpracuj_dbc(lcDbc )
ENDIF

SET TALK ON
RETURN

PROCEDURE zpracuj_dbc
&& =====================
	PARAMETERS pcDBC
	LOCAL i,liPocet ,lcSql
	PUBLIC liHA
	CLOSE DATABASES all
	lcSql = ADDBS(JUSTPATH(pcDBC))+'sql_dump_'+JUSTSTEM(pcDBC)+sys[2015]+'.sql'
	liHa = FCREATE(lcSql )
	OPEN DATABASE (pcDBC) EXCLUSIVE 
	liPocet = ADBOBJECTS(atable,"TABLE")
	FOR i = 1 TO liPocet 
		= zpracuj_tab(atable[i])
		= FWRITE(liHa,CRLF)
	ENDFOR 
	= FCLOSE(liHa)
	CLOSE DATABASES all
ENDPROC

PROCEDURE zpracuj_tab
&& ==================
	PARAMETERS pcTab
	LOCAL i,liPocet ,loZaznam,lcRadek , lcTabString 
	USE (pcTab) IN 0 EXCLUSIVE 
	SELECT (pcTab)
	liPocet = AFIELDS(aPole)
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
*!*	G = General											not possible
*!*	I = Integer											OK
*!*	L = Logical											OK
*!*	M = Memo
*!*	N = Numeric											OK
*!*	Q = Varbinary
*!*	V = Varchar and Varchar (Binary)					OK
*!*	W = Blob
*!*	3 Field width
*!*	4 Decimal places
*!*	5 Null values allowed - Logical
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
*!*	16 Table comment - Character
*!*	17 NextValue for autoincrementing - Numeric
*!*	18 Step for autoincrementing - Numeric
&&
&&
&&
	lcTabString = vytvor_tabulku(pcTab, @aPole)
	= FWRITE(liHa,lcTabString )
	DO WHILE !EOF()
		SCATTER NAME loZaznam
		lcRadek = zpracuj_zaznam(pcTab,@aPole,loZaznam)
		= FWRITE(liHa,lcRadek )
		SKIP +1
		IF RECNO() > 10
			EXIT 
		ENDIF 
	ENDDO 
	USE IN (pcTab)
ENDPROC

PROCEDURE vytvor_tabulku
&& =====================
	PARAMETERS pcTabName, paPole
	LOCAL liPocet , i, lcStr
	liPocet = ALEN(paPole,1)
	lcStr = 'DROP TABLE IF EXISTS ' + ZPUV + pcTabName + ZPUV + ';' + CRLF 
	lcStr = lcStr + 'CREATE TABLE ' + ZPUV + pcTabName + ZPUV + ' ( ' + CRLF 
	FOR i = 1 to liPocet
		DO CASE 
		CASE paPole[i,2] = 'I'
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + " int(11) unsigned NOT NULL default '0' ," + CRLF 
		CASE paPole[i,2] = 'C'
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + " char(" + TRANSFORM(paPole[i,3]) + ") NOT NULL default '' ," + CRLF 
		CASE paPole[i,2] = 'V'
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + " varchar(" + TRANSFORM(paPole[i,3]) + ") NOT NULL default '' ," + CRLF 
		CASE paPole[i,2] = 'N'
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + " decimal(" + TRANSFORM(paPole[i,3]) + "," + TRANSFORM(paPole[i,4]) + ") NOT NULL default '0' ," + CRLF 
		CASE paPole[i,2] = 'D'
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + " date NULL ," + CRLF 
		CASE paPole[i,2] = 'T'
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + " datetime NULL ," + CRLF 
		CASE paPole[i,2] = 'L'
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + " bit NULL ," + CRLF 
		CASE paPole[i,2] = 'B'
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + " double NULL ," + CRLF 
		CASE paPole[i,2] = 'F'
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + " float NULL ," + CRLF 
		CASE paPole[i,2] = 'Y'
			lcStr = lcStr + IND + ZPUV + paPole[i,1] + ZPUV + " decimal(20,4) NOT NULL default '0' ," + CRLF 
		ENDCASE 
	ENDFOR 
	lcStr = LEFT(lcStr,LEN(lcStr)-3)
	lcStr = lcStr + ')' + CRLF 
	lcStr = lcStr + IND + 'ENGINE=MyISAM  DEFAULT CHARSET=utf8 ROW_FORMAT=DYNAMIC;' + CRLF  + CRLF 
	RETURN lcStr 
ENDPROC

PROCEDURE zpracuj_zaznam
&& =====================
	PARAMETERS pcTab,paPole,poZaznam
	LOCAL liPocet ,i,lcSeznamPoli,lcSeznamHodnot,lcRadek 
	STORE '' TO lcSeznamPoli,lcSeznamHodnot
	liPocet = ALEN(paPole,1)
	FOR i = 1 to liPocet
		DO CASE 
		CASE paPole[i,2] = 'I'
			= zpracuj_field(paPole[i,1],paPole[i,2],EVALUATE('pozaznam.'+paPole[i,1]),@lcSeznamPoli,@lcSeznamHodnot)
		CASE paPole[i,2] = 'C'
			= zpracuj_field(paPole[i,1],paPole[i,2],EVALUATE('pozaznam.'+paPole[i,1]),@lcSeznamPoli,@lcSeznamHodnot)
		CASE paPole[i,2] = 'V'
			= zpracuj_field(paPole[i,1],paPole[i,2],EVALUATE('pozaznam.'+paPole[i,1]),@lcSeznamPoli,@lcSeznamHodnot)
		CASE paPole[i,2] = 'N'
			= zpracuj_field(paPole[i,1],paPole[i,2],EVALUATE('pozaznam.'+paPole[i,1]),@lcSeznamPoli,@lcSeznamHodnot)
		CASE paPole[i,2] = 'D'
			= zpracuj_field(paPole[i,1],paPole[i,2],EVALUATE('pozaznam.'+paPole[i,1]),@lcSeznamPoli,@lcSeznamHodnot)
		CASE paPole[i,2] = 'T'
			= zpracuj_field(paPole[i,1],paPole[i,2],EVALUATE('pozaznam.'+paPole[i,1]),@lcSeznamPoli,@lcSeznamHodnot)
		CASE paPole[i,2] = 'L'
			= zpracuj_field(paPole[i,1],paPole[i,2],EVALUATE('pozaznam.'+paPole[i,1]),@lcSeznamPoli,@lcSeznamHodnot)
		CASE paPole[i,2] = 'B'
			= zpracuj_field(paPole[i,1],paPole[i,2],EVALUATE('pozaznam.'+paPole[i,1]),@lcSeznamPoli,@lcSeznamHodnot)
		CASE paPole[i,2] = 'F'
			= zpracuj_field(paPole[i,1],paPole[i,2],EVALUATE('pozaznam.'+paPole[i,1]),@lcSeznamPoli,@lcSeznamHodnot)
		CASE paPole[i,2] = 'Y'
			= zpracuj_field(paPole[i,1],paPole[i,2],EVALUATE('pozaznam.'+paPole[i,1]),@lcSeznamPoli,@lcSeznamHodnot)
		ENDCASE 
	ENDFOR 
	lcRadek = 'INSERT INTO '+ ZPUV + (pcTab) + ZPUV + ' ( ' + lcSeznamPoli + ' )' + ' VALUES ( ' + lcSeznamHodnot + ' ) ' + CRLF
	RETURN lcRadek 
ENDPROC 

PROCEDURE zpracuj_field
&& ====================
	PARAMETERS pcJmeno,pcTyp,peValue,pcSeznamPoli,pcSeznamHodnot

	pcSeznamPoli = pcSeznamPoli + IIF(EMPTY(pcSeznamPoli), ZPUV + pcJmeno + ZPUV, ', ' + ZPUV + pcJmeno + ZPUV)
	DO CASE 
	CASE pcTyp = 'I'
		pcValue = ALLTRIM(TRANSFORM(peValue))
	CASE pcTyp = 'C'
		pcValue = UV + RTRIM(peValue) + UV
	CASE pcTyp = 'V'
		pcValue = UV + RTRIM(peValue) + UV
	CASE pcTyp = 'N'
		pcValue = ALLTRIM(TRANSFORM(peValue))
	CASE pcTyp = 'D'
		pcValue = UV + TRANSFORM(year(peValue)) + '-' + TRANSFORM(MONTH(peValue)) + '-' + TRANSFORM(DAY(peValue)) + UV
	CASE pcTyp = 'T'
		pcValue = UV + TRANSFORM(year(peValue)) + '-' + TRANSFORM(MONTH(peValue)) + '-' + TRANSFORM(DAY(peValue)) + ' ' + ;
			TRANSFORM(HOUR(peValue)) + ':' + TRANSFORM(MINUTE(peValue)) + ':' + TRANSFORM(SEC(peValue)) + UV
	CASE pcTyp = 'L'
		pcValue = iif(peValue,'0b1','0b0')
	CASE pcTyp = 'B'
		pcValue = ALLTRIM(TRANSFORM(peValue))
	CASE pcTyp = 'F'
		pcValue = ALLTRIM(TRANSFORM(peValue))
	CASE pcTyp = 'Y'
		pcValue = ALLTRIM(TRANSFORM(peValue))
		pcValue = RIGHT(pcValue,LEN(pcValue)-1)
	ENDCASE
	pcSeznamHodnot = pcSeznamHodnot + IIF(EMPTY(pcSeznamHodnot),pcValue,', '+pcValue)

ENDPROC
