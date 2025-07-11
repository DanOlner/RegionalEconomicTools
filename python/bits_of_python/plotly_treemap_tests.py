import plotly.express as px
import pandas as pd
import os

os.chdir("/home/dano/Dropbox/YPERN/R/RegionalEcons_web")

#test plotly
#fig = px.scatter(x=[1, 2, 3], y=[4, 5, 6])
#fig.show()

#Load test count data output from R
df = pd.read_csv("local/data/backup/count_output.csv")

fig = px.treemap(
    df,
    path=['SIC_SECTION_NAME_SHORT', 'SIC_2DIGIT_NAME_SHORT', 'SIC_5DIGIT_NAME_SHORT'],
    values='n',
    branchvalues='total'
)

fig.show()

#Now aim to fix colours for each SIC section so they don't change when plot a different place.
colour_map = {
    "Agri": "#1f77b4",               # muted blue
    "Mining/quarrying": "#ff7f0e",   # orange
    "Manuf": "#2ca02c",              # green
    "Power": "#d62728",              # red
    "Water": "#17becf",              # cyan
    "Construction": "#9467bd",       # purple
    "Retail": "#8c564b",             # brown
    "Transport": "#e377c2",          # pink
    "Food/service": "#7f7f7f",       # gray
    "ICT": "#bcbd22",                # yellow-green
    "Finance/insu": "#aec7e8",       # light blue
    "Real est": "#ffbb78",           # light orange
    "Professional/sci/techn": "#98df8a",  # light green
    "Admin/support": "#ff9896",      # salmon
    "Public/defence": "#c5b0d5",     # lavender
    "Education": "#c49c94",          # dusty rose
    "Health/soc": "#f7b6d2",         # rose pink
    "Entertainment": "#dbdb8d",      # pale yellow
    "Other": "#9edae5",              # pale cyan
    "Households": "#393b79",         # dark blue
    "Extraterr": "#636363"           # dark gray
}

fig = px.treemap(
    df,
    path=['SIC_SECTION_NAME_SHORT', 'SIC_2DIGIT_NAME_SHORT', 'SIC_5DIGIT_NAME_SHORT'],
    values='n',
    color='SIC_SECTION_NAME_SHORT',
    color_discrete_map=colour_map,
    branchvalues='total'
)

fig.show()

#fig.write_html("docs/miscdocs/treemap_output.html", full_html=True, include_plotlyjs="cdn")





# REPEAT FOR MULTIPLE LOCAL AUTHORITIES ON SAME PLOT----

#What we're testing here: using those same colours above to keep consistent SIC section colours across places in the same plot

dfch = pd.read_csv("local/data/plotly_dataexportsfromR/CH_count_output_GMLAs.csv")

fig = px.treemap(
    dfch,
    path=['localauthority_name','SIC_SECTION_NAME_SHORT', 'SIC_2DIGIT_NAME_SHORT', 'SIC_5DIGIT_NAME_SHORT'],
    values='n',
    color='SIC_SECTION_NAME_SHORT',
    color_discrete_map=colour_map,
    branchvalues='total'
)

fig.show()

#fig.write_html("docs/miscdocs/WYLAs_CompaniesHouse2025_treemap.html", full_html=True, include_plotlyjs="cdn")





# MULTIPLE LOCAL AUTHORITIES: BRES----

#What we're testing here: using those same colours above to keep consistent SIC section colours across places in the same plot

dflas = pd.read_csv("local/data/backup/count_output_las.csv")

fig = px.treemap(
    dflas,
    path=['GEOGRAPHY_NAME','SIC_SECTION_NAME_SHORT', 'SIC_2DIGIT_NAME_SHORT', 'SIC_5DIGIT_NAME_SHORT'],
    values='n',
    color='SIC_SECTION_NAME_SHORT',
    color_discrete_map=colour_map,
    branchvalues='total'
)

fig.show()

#fig.write_html("docs/miscdocs/WYLAs_BRES_treemap.html", full_html=True, include_plotlyjs="cdn")

