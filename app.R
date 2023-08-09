# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(shinya11y)
library(dplyr)
library(stringr)
library(ggplot2)
library(BrailleR)
library(DT)

# Define Functions ----
freedmanDiaconis <- function(x){
  binwidth <- ifelse(
    test = IQR(x) == 0,
    yes = 0.1,
    no = 2 * IQR(x) / (length(x)^(1/3))
  )
  return(binwidth)
}

# Load Additional Files ----

## Load Data from GitHub ----
centralDataPath <- "https://raw.github.com/neilhatfield/STAT461/master/dataFiles/songKnowledge_"
spring22 <- read.csv(
  file = paste0(centralDataPath, "Spring2022.csv")
)
fall22 <- read.csv(
  file = paste0(centralDataPath, "Fall2022.csv")
)
spring23 <- read.csv(
  file = paste0(centralDataPath, "Sp23.csv")
)
## Load Alt Text----
altText <- read.table(
  file = "altText.csv",
  header = TRUE,
  sep = ",",
  quote = "\"" # Allows for the ' in term
)

# Apply consistent column names and bind into one file
names(spring22) <- c("year", "score")
names(fall22) <- c("year", "score")
names(spring23) <- c("year", "score")
spring22$term <- "Spring '22"
fall22$term <- "Fall '22"
spring23$term <- "Spring '23"
songKnowledgeData <- rbind(spring22, fall22, spring23)
remove(list = c("spring22", "fall22", "spring23"))

# Clean Data
songKnowledgeData <- songKnowledgeData %>%
  mutate(
    year = str_to_sentence(year),
    year = case_when(
      year == "Other" ~ "Sophomore",
      year == "Senior" ~ "Senior+",
      .default = year
    )
  )
songKnowledgeData$year <- factor(
  x = songKnowledgeData$year,
  levels = c("Sophomore", "Junior", "Senior+"),
  ordered = TRUE
)


# Define UI ----
ui <- list(
  dashboardPage(
    skin = "blue",
    ## Header ----
    dashboardHeader(
      title = "Shiny for All",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Shiny_for_All")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("house")
        )
      )
    ),
    ## Sidebar Menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Document Structure", tabName = "docStructure", icon = icon("file-lines")),
        menuItem("Inputs", tabName = "inputs", icon = icon("hand-pointer")),
        menuItem("Tables", tabName = "tables", icon = icon("table")),
        menuItem("Graphics", tabName = "graphics", icon = icon("file-image")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      use_tota11y(),
      tabItems(
        ### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1(
            style = "text-align: center;",
            "Shiny for All:", br(),
            "Making Statistics and Data Science Accessible"
          ),
          p("This Shiny app provides a set of examples for thinking about
            the web accessiblity of Shiny apps. There will be examples of things
            to watch out for and for how to overcome them."),
          h2("Important Note"),
          p("I have not made this app an exhaustive guide for avoiding accessiblity
            issues. I've focused mainly on those issues that I see the most often
            and are [relatively] easily addressible. Additionally, there are
            multiple routes that a programmer can take to achieve accessiblity;
            what I present here is just a few options."),
          h2("App Outline"),
          tags$ol(
            tags$li(strong("Document Structure:"), "Some key HTML tags that will
                    help screenreaders detect a useful structure for your app."),
            tags$li(strong("Inputs:"), "Helping everyone get the most of
                    user inputs."),
            tags$li(strong("Tables:"), "A few details for making accessible tables"),
            tags$li(strong("Graphics:"), "Dealing with color and alt text for
                    static images and plots."),
            tags$li(strong("References:"), "Under construction")
            ),
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Neil J.
            Hatfield",
            br(),
            "I would like to thank Wendy Martinez for inviting me to talk,
            Dennis Pearl for inviting me to get invovled with BOAST and the Shiny
            App program at Penn State, Bob Carey for his advice over the years,
            and all of the undergraduate students involved in the Shiny Program
            program.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 08/08/2023 by NJH.")
          )
        ),
        ### Document Structure Page ----
        tabItem(
          tabName = "docStructure",
          withMathJax(),
          h2("Document Structure"),
          p("Let's explore some basic HTML elements for making an accessible
            Shiny app."),
          #### Page Title and Language ----
          h3("Page Title and Language"),
          p("Setting the page title and the language of your app is key first step.
            Depending on how you're creating your app (e.g., using",
            tags$code("dashboardPage"), "or freestyling), you may be able to use
            an existing argument to set the page title or you'll need to write a
            bit of script to do so.",
            br(),
            br(),
            "Depending upon what manner you're making an app (e.g., ",
            tags$code("fluidPage"), " or ", tags$code("dashboardPage"),") you
            might be able to use the ", tags$code("lang"), "argument to set the
            document language. In BOAST, we created our own launch command,",
            tags$code("boastApp"), ", that automatically injects the page language
            (along with a few other features). Adding the languge to your app can be achieved with
            one line of code:", tags$code("tags$html(lang = 'en')"), "to any part
            of your app's body UI element. For more, check out ",
            tags$a(
              href = "https://www.jumpingrivers.com/blog/accessible-shiny-standards-wcag/",
              class = "bodylinks",
              "Jamie Owen's post"
            ), "."
          ),
          h3("HTML Tags"),
          p("There are many different tags in HTML, each one provides browsers with
            the key information for how to display the elements of your app. The
            three most common areas I see people make avoidable accessiblity
            mistakes are with headings, paragraphs, and lists."),
          #### Heading Tags ----
          h3("Heading Tags"),
          box(
            width = 12,
            title = "Heading Examples",
            collapsed = TRUE,
            collapsible = TRUE,
            h1("Example of Heading 1"),
            h2("Example of Heading 2"),
            h3("Example of Heading 3"),
            h4("Example of Heading 4"),
            h5("Example of Heading 5"),
            h6("Example of Heading 6"),
            p("Example of regular text")
          ),
          p("Heading tags, ", tags$code("h1"), ", ", tags$code("h2"), ", etc. are
            often treated just a way to style text. However, their more fundamental
            purpose is to create a structure for the document. Screenreaders make
            use of this structure to help users better navigate the page."),
          p("When using heading tags, there are several things to keep in mind:"),
          tags$ul(
            tags$li("Don't skip heading levels when moving down through the
                    hieararchy."),
            tags$li("Critically think about whether you really need to use levels
                    4, 5, and 6."),
            tags$li("Given headings are navigational markers, avoid using complete
                    sentences in any one heading. Definitely do not put multiple
                    sentences into a single heading.")
          ),
          #### Paragraphs and Lists ----
          h3("Paragraphs and Lists"),
          p("The vast majority of your text content will appear in either
            paragraphs or lists. If you are writing narrative text, use the
            paragraph tag, ", tags$code("p"), ", to wrap the text. Keep in mind
            the writing rules you've learned back in your elementary school days:
            all sentences in the same paragraph tag should relate to one another.
            When moving onto a new paragraph, close the current tag instance and
            start another; don't use a break, ", tags$code("br"), " to visually
            make a new paragraph. (Note: browsers will automatically add space
            between consecutive paragraph instances.)"),
          p("Sometimes your text content will be be a list. The first thing you
            need to decide is whether you want your reader to think about the
            list as being ordered or unordered. If you want your reader think
            about moving through the list items in a particular order, then you
            need to create an ordered list, ", tags$code("ol"), ". If your reader
            can move through the items however they want, use an unordered list, ",
            tags$code("ul"), ". With the rare exception, all lists items need to
            be in one of these two environments. The environments create a semantic
            relationship between the individual elements. Each list element gets
            wrapped in the list item tag, ", tags$code("li"), ", including
            sublists."),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "structureSW",
              label = "Show bad example",
              icon = icon("triangle-exclamation"),
              size = "large",
              type = "toggle",
              style = "danger",
              value = TRUE
            )
          ),
          br(),
          box(
            width = 12,
            title = strong("Example Document Structure"),
            uiOutput(outputId = "structureExample"),
            collapsible = FALSE,
            collapsed = FALSE
          ),
          fluidRow()
        ),
        ### Inputs ----
        tabItem(
          tabName = "inputs",
          withMathJax(),
          h2("Inputs"),
          p("One of the most powerful aspects of Shiny apps are their ability to
            support user interactions through the notion of interactivity. The
            key to this are the input objects that we can program into each app."),
          p("When designing your inputs, you'll want to make sure that each one
            has a unique name (i.e., ", tags$code("inputId"), ") to ensure that
            things proceed as you intend. (Note: all HTML elements, if named,
            should have unique names.) Further, you'll want to label each input.
            This label is what screenreaders and other assistive technologies
            detect to give meaning to the input. When coming up with your labels,"),
          tags$ul(
            tags$li("Avoid using just an icon. The screenreader will not necessarily
                    provide your user with the meaning you wanted. For example,
                    we might mean 'Caution' with ",
                    HTML("<i class='fa-solid fa-triangle-exclamation'></i>"),
                    "but might skip over the icon or read out 'triangle exclamation'."),
            tags$li("Don't use empty space or set this to a null value."),
            tags$li("Don't place the label somewhere else (e.g., a nearby heading
                    tag) and then set the actual label to empty. This breaks the
                    connection between the label and the input."),
            tags$li("Be clear and useful with your label."),
            tags$li("When creating buttons or other controls, make them as large
                    as you can to make them easier to work with.")
          ),
          fluidRow(
            column(
              width = 6,
              offset = 0,
              div(
                style = "text-align: center;",
                h3("Good Input Examples")
              ),
              numericInput(
                inputId = "repGood",
                label = "Number of replications",
                value = 100,
                min = 10,
                max = 10000,
                step = 10
              ),
              textInput(
                inputId = "textGood",
                label = "Enter your name",
                value = "",
                placeholder = "Family, Given"
              ),
              sliderInput(
                inputId = "sliderGood",
                label = "Sample arithmetic mean, \\(\\mu\\)",
                min = -10,
                max = 10,
                value = 0,
                round = -1
              ),
              bsButton(
                inputId = "buttonGood",
                label = "Simulate",
                icon = icon("person-running"),
                size = "large"
              )
            ),
            column(
              width = 6,
              offset = 0,
              div(
                style = "text-align: center;",
                h3("Bad Input Examples")
              ),
              numericInput(
                inputId = "repBad",
                label = NULL,
                value = 100,
                min = 10,
                max = 10000,
                step = 10
              ),
              p("Enter how many replications you want to do in the above box."),
              textInput(
                inputId = "textBad",
                label = "Family, Given",
                value = ""
              ),
              h3("\\(\\mu\\)"),
              sliderInput(
                inputId = "sliderBad",
                label = " ",
                min = -10,
                max = 10,
                value = 0,
                round = -1
              ),
              bsButton(
                inputId = "buttonBad",
                label = NULL,
                icon = icon("retweet"),
                size = "extra-small"
              )
            )
          )
        ),
        ### Tables ----
        tabItem(
          tabName = "tables",
          withMathJax(),
          h2("Tables"),
          p("Web pages and Tables have a long history. At one point, people used
            nested tables to set the layout of every web page. Such 'layout tables'
            are to be avoided. Rather, we want to make tables that are data tables.
            If you absolutely need a table that is purely for visual aesthetics,
            be sure to set ", tags$code("role='presentation'"), "in the table's
            HTML tag so that screenreaders don't try to announce the text as if
            it was data."),
          p("When making accessible tables, you can do so by hand. Be sure that
            check out ", tags$a(
              href = "https://www.w3.org/WAI/tutorials/tables/",
              class = "bodylinks",
              "W3C's Table Tutorial"), ". You'll need to be sure that you make
            appropriate use of table captions (each table needs one), table head,
            body, and footer, plus table rows, column headers (and scope), and
            table data tags."),
          p("It is better to use a package to build your tables in your app. I
            recommend the ", tags$code("DT"), " package, which has superseded
            the data table function in the ", tags$code("shiny"), " package."),
          DT::dataTableOutput("exampleTable", width = "75%")
        ),
        ### Graphics ----
        tabItem(
          tabName = "graphics",
          withMathJax(),
          h2("Graphics"),
          p("Statistics and Data Science are both heavily visual fields. We need
            to be thoughtful about our visualizations in our materials. If you
            mean to convey some information with a visual element, then you need
            to ensure that you make that element as accessible as possible. This
            means thinking through design elements such as color and font size as
            well as providing alternative descriptions (i.e., alt text). Good
            alt text is"),
          tags$ul(
            tags$li("Short; no more than 100 characters. You can sneak up to
                    150 characters but don't go beyond."),
            tags$li("Avoids waste words/characters. Instead of saying 'a white guy
                    wearing a shirt and tie in a professional photograph', use
                    'headshot of Neil Hatfield'."),
            tags$li("Descriptive. The point of the alt text is to provide a
                    meaningful alternative so that someone who can't see the image
                    draw upon what you intended to convey with it."),
            tags$li("Isn't the file name."),
            tags$li("Contain copyright information. Copyright information goes in
                    a figure caption.")
          ),
          #### Decorative Images ----
          h3("Decorative Images"),
          p("If you are including an image that is purely decorative, this is the
            only time you can set the alt text to an empty string or null (i.e.,",
            tags$code("alt=''"), "). This sends a signal to the browser and
            screenreader that they can safely ignore the image."),
          #### Static Images ----
          h3("Static Images"),
          p("When using static images (e.g., JPEG, PNG, SVG files), it is best
            to use the figure environment rather than just the image tag, ",
            tags$code("img"), ". This will let you set a caption in addition to
            alt text."),
          fluidRow(
            column(
              width = 6,
              h3("Good Static Image Examples"),
              tags$figure(
                class = "centerFigure",
                tags$img(
                  src = "jsm2023Logo.png",
                  width = "75%",
                  alt = "Logo of 2023 Joint Stats Meeting"
                ),
                tags$figcaption("JSM's 2023 Logo")
              ),
              br(), br(),
              tags$figure(
                align = "centerFigure",
                tags$img(
                  src = "astragalus.jpg",
                  width = "75%",
                  alt = "Picture of an astragalus (bone die)"
                ),
                tags$figcaption("Image of Astragalus by Yaan, 2007")
              ),
            ),
            column(
              width = 6,
              h3("Bad Static Image Examples"),
              tags$img(
                src = "jsm2023Logo.png",
                width = '75%',
                alt = "jsm2023Logo.png"
              ),
              br(), br(),
              tags$figure(
                align = "centerFigure",
                tags$img(
                  src = "astragalus.jpg",
                  width = "75%",
                  alt = ""
                ),
                tags$figcaption("Image of Astragalus by Yaan, 2007")
              )
            )
          ),
          #### Plots ----
          h3("Plots"),
          p("The most powerful type of visualization in a Shiny app is a plot
            object that reacts to user inputs. You can use your graphing device
            of choices (mine's ggplot2) within the ", tags$code("renderPlot"),
            "call. Inside this function is the ", tags$code("alt"), " argument
            which lets you set the alternative text. This argument is in a reactive
            environment which let's you update the alt text."),
          p("Keep in mind that statistical graphs get complicated fast and 100
            characters burn up quickly. The prior sentence is 93 characters;
            spaces and puncuation count. For complicated graphs, create a description
            that is located on the page and available to all users. You can use
            the ARIA describedby attribute. For certain graphing packages, you
            can check out the ", tags$code("BrailleR"), " package to auto-generate
            a description."),
          fluidRow(
            column(
              width = 4,
              offset = 0,
              wellPanel(
                selectInput(
                  inputId = "plotType",
                  label = "Select a plot type",
                  choices = c("Box plot", "Histogram", "Density")
                ),
                selectInput(
                  inputId = "termPicked",
                  label = "Select a term",
                  choices = c("Spring '22", "Fall '22", "Spring '23", "All"),
                  selected = "All"
                ),
                checkboxInput(
                  inputId = "byYear",
                  label = "Show data by year in school",
                  value = FALSE
                ),
                bsButton(
                  inputId = "makePlot",
                  label = "Create plot",
                  size = "large",
                  style = "default"
                )
              )
            ),
            column(
              width = 8,
              offset = 0,
              plotOutput(outputId = "songPlot")
            ),
            box(
              title = "BrailleR Description",
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,
              uiOutput(outputId = "braillerDesc"),
              footer = "Caution: text is automatically generated by package."
            ),
            tags$script(HTML(
              "$(document).ready(function() {
                document.getElementById('songPlot').setAttribute(
                'aria-describedby', `braillerDesc`)})"
            ))
          )
        ),
        ### References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p("You'll need to fill in this page with all of the appropriate
            references for your app."),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {

  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "This app will demonstrate accessiblity approaches in Shiny, both
        the good and the bad."
      )
    }
  )

  ## Document Structure Examples ----
  observeEvent(
    eventExpr = input$structureSW,
    handlerExpr = {
      if (input$structureSW) {
        updateButton(
          session = session,
          inputId = "structureSW",
          label = "Show bad example",
          icon = icon("triangle-exclamation"),
          style = "danger"
        )
        output$structureExample <- renderUI(
          expr = {
            tagList(
              h1("Neil's Awesome Report"),
              p(
                "This is my example report where I'll talk about the research
                question", tags$strong("Do double stuf Oreos have twice the creme
                filling mass as regular Oreos?"),
                "I've organized my report as follows:"
              ),
              tags$ol(
                tags$li("History of Oreos and Existing Literature"),
                tags$li("Methodology"),
                tags$li("Sample Description"),
                tags$li("Results"),
                tags$li("Discussion")
              )
            )
          }
        )
      } else {
        updateButton(
          session = session,
          inputId = "structureSW",
          label = "Show good example",
          icon = icon("thumbs-up"),
          style = "success"
        )
        output$structureExample <- renderUI(
          expr = {
            tagList(
              h2("Neil's Terrible Report"),
              p(
                "This is my example report where I'll talk about the research
                question", h4("Do double stuf Oreos have twice the creme
                filling mass as regular Oreos?"),
                "I've organized my report as follows:"
              ),
              tags$li("History of Oreos and Existing Literature"),
              tags$li("Methodology"),
              tags$li("Sample Description"),
              tags$li("Results"),
              tags$li("Discussion")
            )
          }
        )
      }
    },
    ignoreInit = FALSE
  )

  ## Example Table ----
  output$exampleTable <- DT::renderDataTable(
    expr = songKnowledgeData,
    caption = "Stat 461 Song Trivia Scores Over Time",
    style = "bootstrap4",
    rownames = FALSE,
    options = list(
      responsive = TRUE,
      scrollX = TRUE,
      ordering = TRUE,
      paging = TRUE,
      lengthChange = FALSE,
      pageLength = 10,
      searching = FALSE,
      info = TRUE
    )
  )

  ## Song plot ----
  selectedAltText <- reactiveVal("Plot Coming")
  observeEvent(
    eventExpr = input$makePlot,
    handlerExpr = {
      ### Create initial plot object ----
      displayPlot <- "NA"
      ### Filter Data, if necessary
      if (input$termPicked != "All") {
        plotData <- songKnowledgeData %>%
          filter(term == input$termPicked)
      } else {
        plotData <- songKnowledgeData
      }

      ### Base Plot with themeing
      basePlot <- ggplot(
        data = plotData,
        mapping = aes(x = score)
      )

      if (input$byYear) {
        basePlot <- basePlot + aes(fill = year)
      }

      basePlot <- basePlot +
        theme_bw() +
        theme(
          text = element_text(size = 18),
          legend.position = "bottom"
        ) +
        labs(
          x = "Score",
          fill = "Year"
        ) +
        scale_fill_manual(values = psuPalette)

      ### Add Plot Type
      if (input$plotType == "Density" & input$byYear) {
        displayPlot <- plotData %>%
          dplyr::filter(year != "Sophomore") %>%
          ggplot(mapping = aes(x = score, fill = year)) +
          geom_density(
            bounds = c(0, 20),
            alpha = 0.75,
            na.rm = TRUE
          ) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
          theme_bw() +
          theme(
            text = element_text(size = 18),
            legend.position = "bottom"
          ) +
          labs(
            x = "Score",
            fill = "Year",
            caption = "The one Sophomore scoring 8 isn't shown."
          ) +
          scale_fill_manual(values = psuPalette)
      } else if (input$plotType == "Box plot") {
        displayPlot <- basePlot +
          geom_boxplot() +
          theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
          )
      } else if (input$plotType == "Histogram") {
        displayPlot <- basePlot +
          geom_histogram(
            color = "black",
            binwidth = freedmanDiaconis,
            closed = "left",
            boundary = 0,
            position = "identity",
            alpha = 0.75
          ) +
          scale_y_continuous(expand = expansion(add = c(0, 2)))
      } else if (input$plotType == "Density") {
        displayPlot <- basePlot +
          geom_density(
            bounds = c(0, 20),
            alpha = 0.75,
            na.rm = TRUE
          ) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.01)))
      }

      ### Alt text ----
      selectedAltText(
        altText %>%
          filter(
            plotType == input$plotType &
              term == input$termPicked &
              byTerm == input$byYear
          ) %>%
          select(altText) %>%
          as.character()
      )

      ### Display the plot ----
      output$songPlot <- renderPlot(
        expr = {
          validate(
            need(
              expr = !is.na(displayPlot),
              message = "Plot could not be generated. Contact app developer."
            )
          )
          displayPlot
        },
        alt = selectedAltText()
      )

      ### BrailleR Description ----
      output$braillerDesc <- renderUI(
        expr = {
          print(Describe(displayPlot))
          p(Describe(displayPlot)$general)
        }
      )
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
