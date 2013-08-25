LOCAL lcdbc , lodbctools , lcSchemaFile
SET CLASSLIB TO (ADDBS(JUSTPATH(SYS(16,0))) + 'foxpro2mysql')
lcdbc = GETFILE('dbc', 'Database :', 'Select', 0, 'Select database')
lcSchemaFile = LOWER(ADDBS(JUSTPATH(SYS(16,0))) + JUSTSTEM(lcdbc ) + '_schema.sql')
IF !EMPTY(lcdbc )
	lodbctools = CREATEOBJECT( 'dbc_tool', lcdbc )
	IF !ISNULL(lodbctools )
		lodbctools.format_code = .t.
		lodbctools.dbc_scan()
		lodbctools.print_database_schema(lcSchemaFile )
	ENDIF 
ENDIF 
SET CLASSLIB TO 
