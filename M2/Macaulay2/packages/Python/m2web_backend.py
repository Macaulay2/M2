import io
import sys
import base64

# Compatibility import for different Matplotlib versions
try:
    from matplotlib.backend_bases import FigureManagerBase, FigureCanvasBase
except ImportError:
    from matplotlib.backends.backend_bases import FigureManagerBase, FigureCanvasBase

from matplotlib.backends.backend_agg import FigureCanvasAgg
from matplotlib.backends.backend_svg import FigureCanvasSVG

ASCII_START = chr(17)   # DC1
ASCII_END   = chr(18)   # DC2

# GLOBAL CONFIGURABLE OPTION
# Valid values: "png", "svg"
output_format = "svg"     # default


def _emit_html(html):
    sys.stdout.buffer.write((ASCII_START + html + ASCII_END).encode("utf-8"))
    sys.stdout.buffer.flush()


class FigureCanvas(FigureCanvasBase):
    """
    Thin wrapper that delegates to either Agg or SVG canvas depending
    on the chosen output_format.
    """

    def print_m2web(self, figure):
        global output_format

        buf = io.BytesIO()

        # Always create a fresh underlying renderer
        if output_format == "svg":
            real = FigureCanvasSVG(figure)
            real.draw()
            real.print_svg(buf)
            svg = buf.getvalue().decode("utf-8")
            html = f"<div>{svg}</div>"
            _emit_html(html)

        else:  # PNG
            real = FigureCanvasAgg(figure)
            real.draw()
            real.print_png(buf)
            data = base64.b64encode(buf.getvalue()).decode("ascii")
            html = f'<img src="data:image/png;base64,{data}" />'
            _emit_html(html)


class FigureManager(FigureManagerBase):
    pass


def show():
    """
    Replacement for pyplot.show(): render + emit all figures.
    """
    import matplotlib._pylab_helpers as helpers
    for manager in helpers.Gcf.get_all_fig_managers():
        canvas = manager.canvas
        canvas.draw()
        canvas.print_m2web(canvas.figure)


# --- Backend registration hook ---
# Matplotlib expects FigureCanvas to be an actual real renderer subclass.
# We dynamically build wrapper instances here.

def new_figure_manager(num, *args, **kwargs):
    from matplotlib.figure import Figure

    # Matplotlib may pass a custom figure class
    FigureClass = kwargs.pop("FigureClass", Figure)

    # Case 1: pyplot already created the Figure instance
    if args and isinstance(args[0], Figure):
        figure = args[0]

    else:
        # Case 2: backend must construct the figure
        figure = FigureClass(*args, **kwargs)

    # Choose actual underlying renderer (Agg or SVG)
    if output_format == "svg":
        real_canvas = FigureCanvasSVG(figure)
    else:
        real_canvas = FigureCanvasAgg(figure)

    # Wrapper canvas
    canvas = FigureCanvas(figure)

    manager = FigureManager(canvas, num)
    return manager
