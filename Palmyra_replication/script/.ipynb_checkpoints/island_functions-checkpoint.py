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
         desc: bool = True):
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
        
        Returns
        –––––––
        None
        """
        if type(title) == str:
            show(title)
        elif title != None:
            show(title[0], title[1])
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