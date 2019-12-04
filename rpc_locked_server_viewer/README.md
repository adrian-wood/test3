## Locked server viewer

### This displays a summary of RPC servers locked on mdbapus-prod.
 
Uses jinja2 templates.
The following files must be in /var/www/html/viewers/.


 locked_server_viewer.html - this is the entry page with a form for userid.

 viewer_error.html - template error page
   
 viewer_template.html - template viewer page
 
 sorttable.js - javascript referred to in the viewer page to make the table
                sortable by any column.

The following file must be in /var/www/cgi-bin/viewers/.
Check that the script is executable by all.
 
 list_locked_servers.py - script called from the form to parse the server
                          summary page produced on mdbapus-prod.
			  
              This creates a list of locked servers by user and
			  renders the viewer_template.
			  If the summary page on mdbapus-prod is not available
			  it renders the viewer_error page instead.
