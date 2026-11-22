# sfdepmap: step-by-step publishing instructions

Work through the steps in order. Wherever you see `psu-spatial`, use the
GitHub account or organisation you are publishing under.

---

## Step 1. Unzip and edit one file (2 minutes)

1. Unzip `sfdepmap.zip`. You get a folder called `sfdepmap`.
2. Open `sfdepmap/DESCRIPTION` in a text editor.
3. Replace `REPLACE.WITH.YOUR.EMAIL@example.com` with the contact email you
   want shown publicly. Save.

(The licence is MIT: anyone may reuse the code, with credit to you. Tell me
if you want a different one.)

## Step 2. Test it on your own computer (5 minutes)

1. In RStudio, run this in the console, changing the path:

   ```r
   install.packages("path/to/sfdepmap", repos = NULL, type = "source")
   ```

2. Open a Lab 6 or Project 2 script.
3. Delete the line `source("DrGAutocorrelation_Functions.R")`.
4. Add `library(sfdepmap)` to the library chunk.
5. Restart R (Session > Restart R) and run the script.
6. Check these three things, which I could not test:
   - it runs with the real `sfdep` package,
   - the maps look right on real Chicago tracts,
   - `basemap = TRUE` draws a background map.

## Step 3. Put the package on GitHub (5 minutes)

1. Go to github.com and click **New repository**.
2. Owner: `psu-spatial`. Name: `sfdepmap`. Visibility: **Public**.
3. Leave "Add a README", ".gitignore" and "licence" all unticked.
4. Click **Create repository**.
5. Open GitHub Desktop. Choose **File > Add Local Repository**.
6. Choose the `sfdepmap` folder. When it says this is not a repository,
   click **create a repository**, then **Create Repository**.
7. Click **Publish repository**. Choose the `psu-spatial` organisation,
   untick "Keep this code private", and publish.
8. Check: `https://github.com/psu-spatial/sfdepmap` should show the files,
   with `DESCRIPTION` at the top level (not inside another folder).

## Step 4. Make the r-universe registry (5 minutes)

1. On github.com, click **New repository** again.
2. Owner: `psu-spatial`. Name: `psu-spatial.r-universe.dev`
   (the account name in lower case, then `.r-universe.dev`). **Public**.
3. Click **Create repository**.
4. In the new repository, click **creating a new file** (or Add file >
   Create new file).
5. Name the file `packages.json` and paste this in:

   ```json
   [
     {
       "package": "sfdepmap",
       "url": "https://github.com/psu-spatial/sfdepmap"
     }
   ]
   ```

6. Click **Commit changes**.

## Step 5. Switch on r-universe (5 minutes, then waiting)

1. Go to <https://docs.r-universe.dev/publish/set-up.html> and click the
   link to the **r-universe app**.
2. When it asks where to install it, choose `psu-spatial`.
   (For an organisation, you need to be an owner.)
3. Choose **All repositories** and click **Install**.
4. Wait. The first build can take up to an hour or so.
   - Progress: `https://github.com/r-universe/psu-spatial/actions`
   - Finished package: `https://psu-spatial.r-universe.dev`

## Step 6. Test the student install command

Run this on your own computer, then on one student or lab computer:

```r
install.packages("sfdepmap",
                 repos = c("https://psu-spatial.r-universe.dev",
                           "https://cloud.r-project.org"))
library(sfdepmap)
?lisa_map
```

## Step 7. Update the course materials

1. In `sfdepmap/README.md`, replace `YOUR-GITHUB-NAME` in the install
   command, then commit and push.
2. Lab 6, Part B set-up: remove "download `DrGAutocorrelation_Functions.R`
   from Canvas".
3. Lab 6, Step 1B: replace the `source(...)` task with
   - run the `install.packages(...)` command from Step 6 once, in the console,
   - add `library(sfdepmap)` to the library chunk.
4. Lab 6, Step 1B: delete the sentence about a long list of functions
   appearing in the Environment tab. They no longer appear.
5. Search your labs and tutorials for `colours =` inside any `lisa_map()` or
   Getis call, and change it to `palette =`.

---

## Reference

**Functions students use**

| Function | Makes |
|---|---|
| `moran_scatterplot()` | Moran's scatterplot |
| `lisa_map()` | LISA map |
| `getisstar_map()` | Getis-Ord Gi* map |
| `getis_map()` | Getis-Ord Gi map |
| `autocorrelation_summary()` | Four-map figure |

`global_moran_test()` is not in this package. It comes from `sfdep`.

**Changing a function later**

1. Edit the file in `sfdepmap/R/` (captions are in `helpers.R`).
2. Raise the version number in `DESCRIPTION` (for example 0.1.0 to 0.1.1).
3. Commit and push with GitHub Desktop.
4. r-universe rebuilds on its own. Students re-run the install command.

If you add or rename an option, the help pages need regenerating: run
`roxygen2::roxygenise()` in the package folder, or send me the file.
