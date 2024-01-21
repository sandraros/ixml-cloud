interface ZIF_EXCEL_IXML_UNKNOWN
  public .

  methods QUERY_INTERFACE
    importing
      !IID type I
    returning
      value(RVAL) type ref to ZIF_EXCEL_IXML_UNKNOWN .

endinterface.
