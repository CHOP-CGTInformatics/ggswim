from fontTools import ttLib
import pandas as pd
import pyreadr
import os as os


def create_fontawesome_rda():
    # Path to FontAwesome .TTF file
    ttf_path = "inst/fontawesome/solid/fa.ttf"

    # Load the TTF file
    font = ttLib.TTFont(ttf_path)

    # Extract the cmap table
    cmap = font["cmap"]

    # Function to select the best cmap subtable
    def get_best_cmap(cmap_table):
        # Prioritize platformID 3, encodingID 1
        for table in cmap_table.tables:
            if table.platformID == 3 and table.platEncID == 1:
                return table.cmap
        # Fallback to platformID 0 (Unicode)
        for table in cmap_table.tables:
            if table.platformID == 0:
                return table.cmap
        # If no suitable table is found
        return None

    # Get the best cmap
    best_cmap = get_best_cmap(cmap)

    # Check if a suitable cmap was found
    if best_cmap is None:
        raise ValueError("No suitable cmap subtable found in the TTF file.")

    # Extract codepoints and glyph names
    codepoints = []
    glyph_names = []

    for codepoint, glyph_name in best_cmap.items():
        codepoints.append(codepoint)
        glyph_names.append(glyph_name)

    # Generate 'fa' column: actual glyph character
    fa_chars = [chr(cp) for cp in codepoints]

    # Generate 'html' column: HTML entities
    html_entities = [f"&#x{cp:X};" for cp in codepoints]

    # Generate 'aliases' column: glyph names (assuming 'fa-' prefix)
    # Some glyph names might not follow the 'fa-' prefix, so we'll handle them accordingly
    aliases = [gn for gn in glyph_names]

    # Create the dataframe
    df = pd.DataFrame({"fa": fa_chars, "aliases": aliases, "html": html_entities})

    # Out path for rda files
    out_path = "inst/fontawesome/solid/fa.rda"
    df_name = "fa.solid"

    pyreadr.write_rdata(out_path, df, df_name)


create_fontawesome_rda()
