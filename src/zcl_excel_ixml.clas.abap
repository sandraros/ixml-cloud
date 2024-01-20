class ZCL_EXCEL_IXML definition
  public
  final
  create private .

public section.

  interfaces ZIF_EXCEL_IXML .

  class-methods CREATE
    importing
      !TYPE type I default 0
    returning
      value(RVAL) type ref to ZIF_EXCEL_IXML .
      PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_EXCEL_IXML IMPLEMENTATION.


  METHOD create.
    rval = new zcl_excel_ixml( ).
  ENDMETHOD.


  METHOD zif_excel_ixml~create_document.
    rval = NEW lcl_ixml_document( ).
  ENDMETHOD.


  METHOD zif_excel_ixml~create_encoding.
    rval = new lcl_ixml_encoding( ).

  ENDMETHOD.


  METHOD zif_excel_ixml~create_parser.
    rval = lcl_ixml_parser=>create(
             document       = document
             istream        = istream
             stream_factory = stream_factory ).
  ENDMETHOD.


  METHOD zif_excel_ixml~create_renderer.
    rval = new lcl_ixml_renderer( ).
  ENDMETHOD.


  METHOD zif_excel_ixml~create_stream_factory.
    rval = new lcl_ixml_stream_factory( ).
  ENDMETHOD.
ENDCLASS.
