const repoUrl = "https://github.com/weso/srdf";

const apiUrl = "/srdf/api/es/weso/index.html";

// See https://docusaurus.io/docs/site-config for available options.
const siteConfig = {
  title: "SRDF (Simple RDF)",
  tagline: "SRDF",
  url: "https://weso.github.io/srdf",
  baseUrl: "/srdf/",

  customDocsPath: "srdf-docs/target/mdoc",

  projectName: "srdf",
  organizationName: "weso",

  headerLinks: [
    { href: apiUrl, label: "API Docs" },
    { doc: "overview", label: "Documentation" },
    { href: repoUrl, label: "GitHub" }
  ],

  headerIcon: "img/logo-weso.png",
  titleIcon: "img/logo-weso.png",
  favicon: "img/favicon/favicon.ico",

  colors: {
    primaryColor: "#122932",
    secondaryColor: "#153243"
  },

  copyright: `Copyright © 2019-${new Date().getFullYear()} WESO Research group.`,

  highlight: { theme: "github" },

  onPageNav: "separate",

  separateCss: ["api"],

  cleanUrl: true,

  repoUrl,

  apiUrl
};

module.exports = siteConfig;
