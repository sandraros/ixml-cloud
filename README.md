# ixml-cloud
Wrapper of SXML with the old IXML façade

WORK IN PROGRESS (in the branch `needed-by-abap2xlsx`)

This wrapper is currently focused on the classes and methods used by [abap2xlsx](https://github.com/abap2xlsx/abap2xlsx), and the classes are currently named with the prefixes ZCL_EXCEL_IXML\*, ZIF_EXCEL_IXML\* and ZCL_EXCEL_XML_DOCUMENT.

In the end, the objects will be named ZCL_ISXML_\*, ZIF_ISXML_\*, etc. (`ISXML` for combining IXML and SXML).

Steps to replace IXML with ISXML in your code:
- Replace all occurrences of CL_IXML with ZCL_ISXML
- Replace all occurrences of IF_IXML\* with ZIF_ISXML\*
- Replace all occurrences of `stream_factory->create_ostream_???( dobj )` with `stream_factory->create_ostream_???( REF #( dobj ) )`
