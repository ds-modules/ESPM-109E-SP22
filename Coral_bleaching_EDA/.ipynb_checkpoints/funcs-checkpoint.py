#necessary to implement `download_data()` [see below]
import requests
from pathlib import Path
import time
#standard data analysis libraries
import numpy as np 
import pandas as pd 
#imports for displaying, rendering, and saving plots and visualizations
import plotly
import plotly.express as px
from IPython.display import *
import plotly.io as pio
import ipywidgets as widgets
from ipywidgets import *

def visualize(data):
    @interact(Kind = widgets.Dropdown(options=["Scatter Plot", "Histogram", "Box", "Violin"], value = None),
              reset = ToggleButton(value=False, 
                                              description='Return to safety', 
                                              icon = "check", button_style = "success"))
    def plot_kind(reset, Kind):
        if reset:
            return
        cols = widgets.Dropdown(options=data.columns)
        if Kind == "Scatter Plot":
            show(">***NOTE:*** If you chose `Color By` to be a column with numeric data, " \
                 + "that will **disable the `Side Graph`** parameter")
            @interact(x = widgets.Dropdown(options=data.columns, value = None, 
                                           description = "X-Axis"), 
                      y = widgets.Dropdown(options=data.columns, value = None, 
                                           description = "Y-Axis"),
                      color = widgets.Dropdown(options= [None] + list(data.columns), value = None, 
                                               description = "Color By"),
                     marginal = widgets.Dropdown(options = [None, 'rug', 'box', 'violin','histogram'], 
                                                 value = 'histogram', description = "Side Graph"))
            def scatter_helper(x, y, marginal, color):
                if color != None and data[color].dtype == float:
                    marginal = None
                if (x != None and y != None):
                    try:
                        px.scatter(data_frame = data, 
                                   x = x, y = y, 
                                   color = color,
                                   color_continuous_scale='viridis', 
                                   template = 'seaborn',
                                   marginal_x = marginal, marginal_y = marginal,
                                   title = f"{x} vs. {y}").show()
                    except Exception as e:
                        show(x = f"Encountered {e}; please try another combination of selections",
                             tags = ["center", "code style='color:red;font-size:15px'"])
        if Kind == "Histogram":
            @interact(x = widgets.Dropdown(options=data.columns, value = None,
                                          description = "X-Axis"),
                      color = widgets.Dropdown(options=[None] + list(data.columns), value = None,
                                              description = "Color By"),
                     marginal = widgets.Dropdown(options = [None, 'rug', 'box', 'violin','histogram'], 
                                                 value = 'box', description = "Top Graph"))
            def hist_helper(x, marginal, color):
                if (x != None):
                    try:
                        px.histogram(data_frame = data, 
                                   x = x,
                                   color = color, template = "seaborn",
                                    marginal = marginal,
                                    title = f"Distribution of {x}").show()
                    except Exception as e:
                        show(x = f"Encountered {e}; please try another combination of selections",
                             tags = ["center", "code style='color:red;font-size:15px'"])
        if Kind == "Box":
            @interact(x = widgets.Dropdown(options=data.columns, value = None,
                                          description = "X-Axis"),
                      y = widgets.Dropdown(options=data.columns, value = None,
                                          description = "Y-Axis"),
                      color = widgets.Dropdown(options=[None] + list(data.columns), value = None,
                                              description = "Color By"),
                     notches = Dropdown(value = False, options= [True, False], description="Notched Style"))
            def box_helper(x, y, color, notches):
                if (x != None):
                    try:
                        px.box(data_frame = data,
                               x = x, y = y, 
                               color = color, template = "seaborn",
                               notched = notches).show()
                    except Exception as e:
                            show(x = f"Encountered {e}; please try another combination of selections",
                                 tags = ["center", "code style='color:red;font-size:15px'"])
        if Kind == "Violin":
            @interact(x = widgets.Dropdown(options=data.columns, value = None,
                                          description = "X-Axis"),
                      y = widgets.Dropdown(options=data.columns, value = None,
                                          description = "Y-Axis"),
                      color = widgets.Dropdown(options=[None] + list(data.columns), value = None,
                                              description = "Color By"),
                     box = Dropdown(value = False, options= [True, False], description="Inner Boxes"))
            def violin_helper(x, y, color, box):
                if (x != None):
                    try:
                        px.violin(data_frame = data,
                                  x = x, y = y, 
                                  color = color, template = "seaborn",
                                  box = box).show()
                    except Exception as e:
                            show(x = f"Encountered {e}; please try another combination of selections",
                                 tags = ["center", "code style='color:red;font-size:15px'"])


def show(x : str = None, 
         tags: list = []):
    """
    Display text or other data using Ipython
    
    Parameters
    ––––––––––
    x : str | default ``None``
        the value to display, if None,
        two empty lines are displayed
        
    tags : list of str | default ``[]``
        uses each element of tags as an HTML
        tag; tags will be applied from left
        to right, so the last tage in the 
        list will be the outermost applied
    
    Returns
    –––––––
    None
    """
    for tag in tags:
        x = f"<{tag}>{x}</{tag}>"
    display(Markdown(" "))
    display(Markdown(x))
    display(Markdown(" "))
    
def showtable(self, 
         allrows: bool = False, 
         columns: list = ["all"], 
         rows: int = 20, 
         start: int = 0,
         title: str = None,
         desc: bool = True,
         interact: bool = False):
        """
        Display pandas.DataFrame using custom values 

        Parameters
        ––––––––––
        allrows : bool | default ``False``
            Wether or not to show all rows
            
        columns : list | default ``["all"]``
            Default shows all columns. Set to list of 
            column names to select those columns
            
        rows : int | default ``20``
            How mant rows of the DataFrame to display.
            If rows < 0, displays the last 
            ``abs(rows)`` entries
            
        start: int | default ``0``
            What index to start displaying the DataFrame at
        
        title: str | default ``None``
            A title for the DataFrame to be displayed using
            ``show()``
        desc: bool | default ``True``
            Wether to display the DataFrame's size
            
        interact: bool | default ``True``
            Wether to show an interactive table using widgets
        
        Returns
        –––––––
        None
        """
        if type(title) == str:
            show(title)
        elif title != None:
            show(title[0], title[1])
        if interact:
            @interact(allrows = ToggleButton(value=False, description='Show ALL Rows', 
                                             icon = "eye", button_style = "warning"),
                      rows = BoundedIntText(min = 1, max = len(self), value = 5),
                      columns = SelectMultiple(options=list(self.columns), 
                                                 value=list(self.columns), description='Columns:'))
            def helper_top(allrows, columns, rows):
                @interact(start = BoundedIntText(min = 1, max = len(self) - rows, value = 0,
                                                 description = "Start Row"))
                def helper(start):
                    showtable(self, allrows, columns, rows, start)
            return
        settings = ['display.max_rows','display.max_columns',
                        'display.width','display.max_colwidth']
        [pd.set_option(i, None) for i in settings]
        loc, cols = 'head', self.columns
        if rows < 0:
            loc = 'tail'
        if columns != ["all"]:
            cols = columns
        if allrows:
            display(self[cols])
        if start or columns != ['all']:
            display(self[cols].iloc[start:start+rows , :])
        else:
            eval(f"display(self.{loc}({abs(rows)}))")
        [pd.reset_option(i) for i in settings]
        if desc:
            nrow, ncol = self.shape
            show(f"{nrow} Rows x {ncol} Columns", [])
pd.DataFrame.show = showtable