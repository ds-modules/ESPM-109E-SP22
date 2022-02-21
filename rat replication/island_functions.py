def show(x, tags = []):
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
         title = None,
         desc = True):
        """
        Display pandas.DataFrame using custom values 

        Parameters
        ----------
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
        
        Returns
        -------
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