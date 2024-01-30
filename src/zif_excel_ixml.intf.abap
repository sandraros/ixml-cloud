INTERFACE zif_excel_ixml
  PUBLIC.

  METHODS create_document
    RETURNING
      VALUE(rval) TYPE REF TO zif_excel_ixml_document.

  METHODS create_encoding
    IMPORTING
      !byte_order TYPE i
      !character_set TYPE string
    RETURNING
      VALUE(rval) TYPE REF TO zif_excel_ixml_encoding.

  METHODS create_parser
    IMPORTING
      !document TYPE REF TO zif_excel_ixml_document
      !istream TYPE REF TO zif_excel_ixml_istream
      !stream_factory TYPE REF TO zif_excel_ixml_stream_factory
    RETURNING
      VALUE(rval) TYPE REF TO zif_excel_ixml_parser.

  METHODS create_renderer
    IMPORTING
      !document TYPE REF TO zif_excel_ixml_document
      !ostream TYPE REF TO zif_excel_ixml_ostream
    RETURNING
      VALUE(rval) TYPE REF TO zif_excel_ixml_renderer.

  METHODS create_stream_factory
    RETURNING
      VALUE(rval) TYPE REF TO zif_excel_ixml_stream_factory.

ENDINTERFACE.
