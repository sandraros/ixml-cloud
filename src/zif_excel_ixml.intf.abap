INTERFACE zif_excel_ixml
  PUBLIC .

  methods CREATE_DOCUMENT
    returning
      value(RVAL) type ref to zIF_excel_IXML_DOCUMENT .

  methods CREATE_ENCODING
    importing
      !BYTE_ORDER type I
      !CHARACTER_SET type STRING
    returning
      value(RVAL) type ref to zIF_excel_IXML_ENCODING .

  methods CREATE_PARSER
    importing
      !DOCUMENT type ref to zIF_excel_IXML_DOCUMENT
      !ISTREAM type ref to zIF_excel_IXML_ISTREAM
      !STREAM_FACTORY type ref to zIF_excel_IXML_STREAM_FACTORY
    returning
      value(RVAL) type ref to zIF_excel_IXML_PARSER .

  methods CREATE_RENDERER
    importing
      !DOCUMENT type ref to zIF_excel_IXML_DOCUMENT
      !OSTREAM type ref to zIF_excel_IXML_OSTREAM
    returning
      value(RVAL) type ref to zIF_excel_IXML_RENDERER .

  methods CREATE_STREAM_FACTORY
    returning
      value(RVAL) type ref to zIF_excel_IXML_STREAM_FACTORY .

ENDINTERFACE.
