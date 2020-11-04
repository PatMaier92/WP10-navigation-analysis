function DN=datanorm(c,cmin,cmax)
% DATANORM Data-normalization for coordinates.
%
% Input: 
% c is input value to be normalized.
% cmin, cmax are minumum, maximum values.
%
% Returns: DN is normalized value. 

DN=((c-cmin)/(cmax-cmin));


