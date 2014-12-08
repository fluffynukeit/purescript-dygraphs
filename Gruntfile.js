module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs",
    ],

    clean: ["tmp", "output"],

    pscMake: ["<%=libFiles%>"],
    dotPsci: ["<%=libFiles%>"],
    pscDocs: {
        readme: {
            src: "src/**/*.purs",
            dest: "docs/Module.md"
        }
    },

    psc: {
      exampleTest: {
        options: { main: "Examples", modules: "Examples" },
        src: ["<%=libFiles%>", "examples/examples.purs"],
        dest: "tmp/Examples.js"
      }
    },

    copy: [
      {
        expand: true,
        cwd: "node_modules",
        src: "**",
        dest: "tmp/node_modules/"
      }
    ]

    
  }
  );

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-execute");
  grunt.loadNpmTasks("grunt-contrib-copy");

  grunt.registerTask("make", ["pscMake", "dotPsci", "pscDocs"]);
  grunt.registerTask("default", ["clean", "make", "examples"]);
  grunt.registerTask("examples", ["psc", "copy"]);
};

