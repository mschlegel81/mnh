UNIT mnh_documenter;
USES mnh_funcs;
TYPE
  T_infoBlock=object
    
    CONSTRUCTOR create(CONST blockTitle:ansistring; CONST filename:ansistring);
    DESTRUCTOR destroy;
    PROCEDURE append(CONST s:string);
  end;
INITIALIZATION

FINALIZATION
  activePlot.Destroy;
end.
