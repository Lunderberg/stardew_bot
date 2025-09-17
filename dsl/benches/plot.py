#!/usr/bin/env python3

import argparse
import contextlib
import json
import pathlib
import sys

import matplotlib.pyplot as plt
import pandas as pd
import numpy as np


def main(args):
    result_dir = pathlib.Path(__file__).parent.parent.parent.joinpath(
        "target", "criterion"
    )
    rows = []
    for filepath in result_dir.glob("*/*/new/estimates.json"):
        step, test_case, *_rest = filepath.relative_to(result_dir).parts
        estimates = json.load(filepath.open())
        rows.append(
            {
                "step": step,
                "test_case": test_case,
                "time": pd.to_timedelta(estimates["mean"]["point_estimate"], unit="ns"),
            }
        )
    df = pd.DataFrame(rows).pivot_table(
        index="test_case", values="time", columns="step"
    )

    fig, axes_list = plt.subplots(ncols=2, figsize=(8, 6))

    norm = plt.Normalize(1, 4)
    cmap = plt.cm.RdYlGn
    np.random.seed(0)
    colors = np.random.randint(1, 5, size=len(df))

    scatterplots = []
    scatterplots.append(
        axes_list[0].scatter(
            df.execution_vm,
            df.execution_interpreter,
            c=colors,
        )
    )
    axes_list[0].set_xlabel("VM runtime (ns)")
    axes_list[0].set_ylabel("Interpreter runtime (ns)")

    scatterplots.append(
        axes_list[1].scatter(
            df.execution_opt_vm,
            df.execution_opt_interpreter,
            c=colors,
        )
    )
    axes_list[1].set_xlabel("Opt. VM runtime (ns)")
    axes_list[1].set_ylabel("Opt. Interpreter runtime (ns)")

    annotations = []
    for axes in axes_list:
        axes.set_xscale("log")
        axes.set_yscale("log")

        lims = [
            np.min([axes.get_xlim(), axes.get_ylim()]),
            np.max([axes.get_xlim(), axes.get_ylim()]),
        ]
        axes.plot(lims, lims, "k-", alpha=0.75, zorder=0)

        axes.set_aspect("equal")
        axes.set_xlim(lims)
        axes.set_ylim(lims)

        annot = axes.annotate(
            "",
            xy=(0, 0),
            xytext=(20, 20),
            textcoords="offset points",
            bbox=dict(boxstyle="round", fc="w"),
            arrowprops=dict(arrowstyle="->"),
        )
        annot.set_visible(False)
        annotations.append(annot)

    fig.tight_layout()

    def update_annot(sc, annot, ind):
        pos = sc.get_offsets()[ind["ind"][0]]
        annot.xy = pos
        text = " ".join([df.iloc[i].name for i in ind["ind"]])
        annot.set_text(text)
        annot.get_bbox_patch().set_facecolor(cmap(norm(colors[ind["ind"][0]])))
        annot.get_bbox_patch().set_alpha(0.4)

    def hover(event):
        for sc, annot, ax in zip(scatterplots, annotations, axes_list):
            if event.inaxes != ax:
                continue

            cont, ind = sc.contains(event)
            if cont:
                update_annot(sc, annot, ind)
                annot.set_visible(True)
                fig.canvas.draw_idle()
            elif annot.get_visible():
                annot.set_visible(False)
                fig.canvas.draw_idle()

    if args.output:
        fig.savefig(args.output)
    else:
        fig.canvas.mpl_connect("motion_notify_event", hover)
        plt.show()


@contextlib.contextmanager
def debug_on_except():
    try:
        yield
    finally:
        if isinstance(sys.exc_info()[1], Exception):
            import traceback

            try:
                import ipdb as pdb
            except ImportError:
                import pdb

            traceback.print_exc()
            pdb.post_mortem()


def arg_main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-o",
        "--output",
        type=pathlib.Path,
        default=None,
        help="Output path to save the generated plot",
    )
    parser.add_argument(
        "--pdb",
        action="store_true",
        help="Start a pdb post mortem on uncaught exception",
    )

    args = parser.parse_args()

    with contextlib.ExitStack() as stack:
        if args.pdb:
            stack.enter_context(debug_on_except())

        main(args)


if __name__ == "__main__":
    arg_main()
