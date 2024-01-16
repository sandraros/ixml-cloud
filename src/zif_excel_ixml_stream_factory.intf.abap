INTERFACE zif_excel_ixml_stream_factory
  PUBLIC .

  methods CREATE_ISTREAM_STRING
    importing
      !STRING type STRING
    returning
      value(RVAL) type ref to zIF_excel_IXML_ISTREAM .

  methods CREATE_ISTREAM_XSTRING
    importing
      !STRING type XSTRING
    returning
      value(RVAL) type ref to zIF_excel_IXML_ISTREAM .

  methods CREATE_OSTREAM_CSTRING
    importing
      !STRING type STRING
    returning
      value(RVAL) type ref to zIF_excel_IXML_OSTREAM .

  methods CREATE_OSTREAM_XSTRING
    importing
      !STRING type XSTRING
    returning
      value(RVAL) type ref to zIF_excel_IXML_OSTREAM .

ENDINTERFACE.
