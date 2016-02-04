Viewer Applications
===================

1) Locked server viewer

This displays a summary of RPC servers locked on mdbapus-prod.
 
Uses jinja2 templates.
The following files must be in /home/h01/usmdb/public_html/moods/viewers/.
Check that the script is executable by all.


 locked_server_viewer.html - this is the entry page with a form for userid.
 
 list_locked_servers.py - script called from the form to parse the server
                          summary page produced on mdbapus-prod.
			  This creates a list of locked servers by user and
			  renders the viewer_template.
			  If the summary page on mdbapus-prod is not available
			  it renders the viewer_error page instead.

 viewer_error.html - template error page

 viewer_template.html - template viewer page

 sorttable.js - javascript referred to in the viewer page to make the table
                sortable by any column.

