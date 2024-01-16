CLASS zcl_excel_xml_document DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

  data M_DOCUMENT type ref to zIF_excel_IXML_DOCUMENT read-only .

  methods PARSE_STRING
    importing
      !STREAM type STRING
    returning
      value(RETCODE) type SYSUBRC .

  methods PARSE_XSTRING
    importing
      !STREAM type XSTRING
    returning
      value(RETCODE) type SYSUBRC .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_excel_xml_document IMPLEMENTATION.
  METHOD parse_xstring.

  ENDMETHOD.

  METHOD parse_string.

  ENDMETHOD.

ENDCLASS.
