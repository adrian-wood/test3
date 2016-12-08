def daily_archive(symLink, baseDir, page):
    """Write a HTML page to a location based on the current date and create
    a symbolic link to it.

    This will usually be used for scripts which run on a regular basis,
    creating output that needs to be stored in an "archive" directory, with
    a link to the "current" page.
    The actual page will be created in a file named <dd>.html in subdirectories
    <yyyy>/<mm> under the baseDir argument, which are created if necessary.
    The symbolic link supplied in the symlink argument will then be destroyed
    and re-created, linking to the file written.
    The contents of the page should be supplied in the "page" argument.

    Args:
        symLink: the fully-qualified path to the symlin to be re-created
        baseDir: the fully-qualified base directory of the "archive"
        page: the HTML output to be written to the file

    Returns:
        Nothing
    """
    import os
    from datetime import datetime
    now = datetime.now()
    outDir = baseDir + now.strftime("/%Y/%m")

    if not os.path.exists(outDir):
        os.makedirs(outDir)

    outFile = outDir + now.strftime("/%d") + ".html"

    with open(outFile, "w") as outp:
        outp.write(page)
        # Recreate the symlink to the latest file
        os.unlink(symLink)
        os.symlink(outFile, symLink)
