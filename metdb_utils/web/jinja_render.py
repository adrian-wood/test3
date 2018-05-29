def jinja_render(directory, template_name, header='', **kwargs):
    """Render a web page using a Jinja2 template.

    Args:
        directory: the directory where the template is located.
        template_name: name of the Jinja2 template file.
        header: optional content-type text field - required if calling from cgi
             script.
        **kwargs: dictionary of keyword arguments used by the Jinja2 template.

     Returns:
         output: the rendered HTML output, which can then be written to a file.
    """
    from jinja2 import FileSystemLoader, Environment
    loader = FileSystemLoader(directory)
    env = Environment(loader=loader)
    template = env.get_template(template_name)
    output = header + template.render(**kwargs)
    return output
