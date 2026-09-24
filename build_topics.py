# build_topics.py
# Quarto pre-render script. Scans module*.qmd and project.qmd for topic
# headings and writes _topics-data.html, a <script> that defines
# window.STA510_TOPICS. The dashboard, the sidebar counters and the status
# buttons all read from it.
#
# Heading format (level 2 only):
#   ## 3.9 R-hat {#t3-9 .topic flag="ilo project"}
#   ## 2.4 Flat priors {#t2-4 .topic}
#   ## Step 4 · Priors and a prior predictive check {#p4 .topic .step flag="project"}
#
# flag is a space-separated list of: ilo, project
# .step marks a project step (labels Not started / In progress / Done)

import glob
import json
import os
import re

HEADING = re.compile(
    r'^## (.+?)\s*\{#(\S+)\s+\.topic((?:\s+\.[\w-]+)*)(?:\s+flag="([^"]*)")?\s*\}\s*$'
)

here = os.path.dirname(os.path.abspath(__file__))
topics = []

module_files = sorted(glob.glob(os.path.join(here, "module*.qmd")),
                      key=lambda f: int(re.search(r"module(\d+)", f).group(1)))
files = module_files + [os.path.join(here, "project.qmd")]

for path in files:
    if not os.path.exists(path):
        continue
    match = re.search(r"module(\d+)", os.path.basename(path))
    module = int(match.group(1)) if match else 7          # 7 = the project
    page = os.path.basename(path).replace(".qmd", ".html")
    with open(path, encoding="utf-8") as fh:
        for line in fh:
            m = HEADING.match(line.rstrip("\n"))
            if m:
                classes = m.group(3).split()
                flags = m.group(4).split() if m.group(4) else []
                topics.append({
                    "id": m.group(2),
                    "title": m.group(1),
                    "module": module,
                    "page": page,
                    "flags": flags,
                    "kind": "step" if ".step" in classes else "topic",
                })

out = os.path.join(here, "_topics-data.html")
with open(out, "w", encoding="utf-8") as fh:
    fh.write("<script>\nwindow.STA510_TOPICS = ")
    fh.write(json.dumps(topics, ensure_ascii=False, indent=1))
    fh.write(";\n</script>\n")

print(f"build_topics.py: {len(topics)} topics from {len(files)} files")
