Website for the SCO-SOC Virtual Meeting 2026

**October 5-7, 2026**


## Making changes

There are several ways to make changes (simple to complex):

- Open an [Issue](https://github.com/SCO-SOC/Virtual2026/issues/new?template=edit-a-page.md) to outline the changes you'd like made (Steffi will be notified automatically)
- Email English and French text to Steffi (<sel@steffilazerte.ca>)
- Make the proposed edits yourself [directly on the file through the browser](https://docs.github.com/en/repositories/working-with-files/managing-files/editing-files). You'll need to know some Markdown and have a free GitHub account
- Fork the repository and make the proposed edits yourself then create a PR
- Contact Steffi and join the Website Team to make changes yourself as needed.


## Site Design and Details

### Updating the site
- Note that there are two version English (`en`) and French (`fr`), both should be updated.
- Updates can be previewed locally by running `quarto render en` or `quarto render fr` (or `quarto render en; quarto render fr` to do both at once) in the terminal and then looking at the html files in `docs`
- The `index.html` file contains an automatic redirect to the `en` version (i.e. if people navigate to https://sco-soc.github.io/

### Multilingual 

Two websites under `en` and `fr`, which can be toggled back and forth via the 
Navigation Menu (EN/FR) (right side). 

Following examples from:

- https://github.com/quarto-dev/quarto-cli/issues/275#issuecomment-3248104124
- https://github.com/guillemmaya92/guillemmaya92.github.io/blob/main/cn/_quarto.yml
- https://guillemmaya.com/

To render this site install [Quarto](https://quarto.org) and then in the *terminal* navigate to the main folder and use `quarto render en; quarto render fr` (renders both en and fr versions).

Note that all images must be duplicated in each folder (i.e. `en/figs` and `fr/figs`) because the projects need images to be in the folder.