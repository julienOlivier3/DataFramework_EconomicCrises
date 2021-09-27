setwd("Q:\\Meine Bibliotheken\\Research\\01_Promotion\\05_Ideas\\07_COVID_Tracking")
source(file.path(getwd(), '04_Code', 'setup.R'))


# Timeline information:
lockdown_date <- dmy('16-03-2020') # announcement of first lockdown
#lockdown_date <- dmy('22-03-2020') # start of first lockdown
#lockdown_date <- dmy('28-01-2020') # first COVID case

# - 


library(DiagrammeR)
viridis_map <- viridis.map %>% as_tibble()

viridis_map_E <- viridis_map %>% filter(opt=="E")
rgb(viridis_map_E$R, viridis_map_E$G, viridis_map_E$B, maxColorValue = 360)



grViz("
    digraph data_viz {
    
    
    forcelabels=true;
        
        # oval node statements
        node [shape = invhouse,
              fontname = ModernComputer,
              width = 1,
              penwidth = 2,
              color = grey]
        i5 [label = 'Economic \n shock']
        
        
    
        # arrow node statements
        node [shape = plaintext,
              fontwidth = 2,
              fontname = ModernComputer,
              width = 0.25,
              penwidth = 1,
              color = grey]
        i6 [label = < Time >]
    
        # arrow node statements
        node [shape = plaintext,
              fontwidth = 2,
              fontname = ModernComputer,
              width = 1.5,
              penwidth = 2,
              color = grey]
        a1 [label = < <B>Ad hoc analysis</B> >]
        a2 [label = < <B>Follow-up analysis</B> >]
        a3 [label = < <B>Retrospective <br/> liquidation risk analysis</B> >]


        # box node statements
        node [shape = cylinder,
              fontname = ModernComputer,
              fontsize=15,
              height = 1,
              width = 3,
              penwidth = 2,
              color = grey]
        b1 [label = <Corporate website data>]
        b2 [label = <Survey data>]
        b3 [label = <Credit rating data>]
        
        
        # arrow node statements
        node [shape = plaintext,
              fontwidth = 2,
              fontsize=20,
              fontname = ModernComputer,
              width = 0.25,
              penwidth = 2,
              color = grey]
        i7 [label = <  ->]
        i8 [label = <  +>]
        i9 [label = <  ->]
        i10 [label = <  +>]
        
        
        # invisible node statements
        node [style = invis,
              fontwidth = 2,
              fontname = ModernComputer,
              width = 0.25,
              penwidth = 2]
        i1
        i2
        i3
        i4




        # edge statements

        
        
        i5 -> i6 [arrowhead = vee, arrowtail = inv]
        i7 -> i8 [
        minlen=2
        labeldistance=20
        labelangle=0
        headlabel=<
                    <table bgcolor='white' border='0'>
                      <tr>
                        <td>Cost</td>
                      </tr>
                    </table>
                  >
              ]
        i9 -> i10 [
        minlen=2
        labeldistance=20
        labelangle=0
        headlabel=<
                    <table bgcolor='white' border='0'>
                      <tr>
                        <td>Depth</td>
                      </tr>
                    </table>
                  >
              ]
              
        
        
        i1 -> a1   [style = invis ]
        a3 -> i2   [style = invis ]
        i3 -> b1   [style = invis ]
        b3 -> i4   [style = invis ]
        
        a1 -> a2 [style = invis ]
        a2 -> a3 [style = invis ]
              
        b1 -> b2 
        b2 -> b3



        # define ranks

        subgraph {
            rank = same; a1; b1
        }
        subgraph {
            rank = same; a2; b2
        }
        subgraph {
            rank = same; i1; i3; i5; i7; i9
        }
        subgraph {
            rank = same; i2; i4; i6; i8, i10
        }
        
        
        
    }
")
