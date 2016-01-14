(defproject org.clojars.clojure-first-class-objects/firstclassobjects "0.0.2"
  :description "A pilot project to use Clojure for introductory computer science courses at the University of Minnesota - Morris"
  :url "https://github.com/Clojure-Intro-Course/Clojure-FirstClassObjects.git"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as Clojure"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [quil "2.2.5"]
                 [inflections "0.9.14"]]
  :plugins [[lein-autoexpect "1.0"]
            [lein-pprint "1.1.2"]]
  :scm {:url "https://github.com/Clojure-Intro-Course/Clojure-FirstClassObjects.git"}
  :pom-addition [:developers [:developer
                              [:name "Thomas Hagen"]
                              [:email "hagen715@morris.umn.edu"]
                              [:timezone "-6"]]]
    :main firstclassobjects.core)
