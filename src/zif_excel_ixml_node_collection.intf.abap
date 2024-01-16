INTERFACE zif_excel_ixml_node_collection
  PUBLIC .

  methods CREATE_ITERATOR
    returning
      value(RVAL) type ref to zIF_excel_IXML_NODE_ITERATOR .

  methods GET_LENGTH
    returning
      value(RVAL) type I .

ENDINTERFACE.
