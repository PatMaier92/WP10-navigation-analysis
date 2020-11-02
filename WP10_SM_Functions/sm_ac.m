% calculation accuracy
function A= sm_ac(i_total,i_ideal)
A=((i_total-i_ideal)/i_ideal)*100;
                if A <=0
                    A=A*(-1);
                end