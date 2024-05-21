package asl
 import _root_.scala.collection.mutable.HashMap

 import _root_.akka.actor.typed.{ActorRef, Behavior, SupervisorStrategy}
 import _root_.akka.actor.typed.scaladsl.{ActorContext, Behaviors, Routers}
 import java.util.logging.Logger
 import _root_.scala.util.Failure
 import _root_.scala.util.Success
 import _root_.akka.util.Timeout
 import _root_.scala.concurrent.duration._
 import _root_.akka.actor.typed.scaladsl.AskPattern._
 import _root_.scala.language.implicitConversions
 import _root_.scala.concurrent.{Await, Future}
 import _root_.scala.jdk.CollectionConverters._
 import std.converters._

 import scala.util.Random
 import bb._
 import infrastructure._
 import bb.expstyla.exp._
 import std.{AgentCommunicationLayer, DefaultCommunications}

 class test  (coms: AgentCommunicationLayer = new  DefaultCommunications,
                                     beliefBaseFactory: IBeliefBaseFactory = new StylaBeliefBaseFactory)
                      extends IntentionalAgentFactory {


 object Intention {

       def apply(p_executionContext: ExecutionContext): Behavior[ISubGoalMessage] = Behaviors.setup { context =>

         Behaviors.receive {
         (context, message) =>

          implicit val executionContext = p_executionContext.copy(intention = context, src = message.source)

         message match {
            case SubGoalMessage(_,_,r) =>
               message.goal match {

                   case test.this.adopt_achievement_getnames_0 =>
                     test.this.adopt_achievement_getnames_0.execute(message.params.asInstanceOf[Parameters])

                   case test.this.adopt_achievement_init_1 =>
                     test.this.adopt_achievement_init_1.execute(message.params.asInstanceOf[Parameters])

                   case test.this.adopt_achievement_distanceSum_2 =>
                     test.this.adopt_achievement_distanceSum_2.execute(message.params.asInstanceOf[Parameters])

                   case test.this.adopt_achievement_distanceWeight_2 =>
                     test.this.adopt_achievement_distanceWeight_2.execute(message.params.asInstanceOf[Parameters])

                   case test.this.adopt_achievement_normalizedDistance_1 =>
                     test.this.adopt_achievement_normalizedDistance_1.execute(message.params.asInstanceOf[Parameters])


           case _ =>
             context.log.error("This actor can not handle goal of type {}", message.goal)
         }
           r match {
                 case a : AkkaMessageSource => 
                   a.src ! IntentionDoneMessage(AkkaMessageSource(executionContext.agent.self))
                 case DummyMessageSource(_) => 
                   context.log.error("Intention Done!")
               }

               Behaviors.same
             case InitEndMessage(r) =>
               Behaviors.stopped
       }
      }
     }
     }

 override def agentBuilder: Agent = new Agent()
 class Agent extends IAgent {

         override def agent_type: String = "test"

         var vars = VarMap()

         def initGoals()(implicit executionContext: ExecutionContext) = List[StructTerm](
                     StructTerm("getnames",Seq[GenericTerm](  ))


         )

         def initBeliefs()(implicit executionContext: ExecutionContext) = List[StructTerm](
                     StructTerm("agent",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]())))
           ,
            StructTerm("agent",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]())))
           ,
            StructTerm("principle",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("p1",Seq[GenericTerm]()),IntTerm(0)))
           ,
            StructTerm("principle",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("p2",Seq[GenericTerm]()),IntTerm(0)))
           ,
            StructTerm("principle",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("p3",Seq[GenericTerm]()),IntTerm(0)))
           ,
            StructTerm("principle",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("p4",Seq[GenericTerm]()),IntTerm(0)))
           ,
            StructTerm("principle",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("p5",Seq[GenericTerm]()),IntTerm(0)))
           ,
            StructTerm("intention",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("p1",Seq[GenericTerm]()),DoubleTerm(0.6)))
           ,
            StructTerm("intention",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("p2",Seq[GenericTerm]()),DoubleTerm(0.9)))
           ,
            StructTerm("intention",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("p3",Seq[GenericTerm]()),DoubleTerm(0.4)))
           ,
            StructTerm("intention",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("p4",Seq[GenericTerm]()),DoubleTerm(0.9)))
           ,
            StructTerm("intention",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("p5",Seq[GenericTerm]()),DoubleTerm(0.2)))
           ,
            StructTerm("weight",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("p1",Seq[GenericTerm]()),DoubleTerm(0.7)))
           ,
            StructTerm("weight",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("p2",Seq[GenericTerm]()),DoubleTerm(0.8)))
           ,
            StructTerm("weight",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("p3",Seq[GenericTerm]()),DoubleTerm(0.4)))
           ,
            StructTerm("weight",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("p4",Seq[GenericTerm]()),DoubleTerm(0.4)))
           ,
            StructTerm("weight",Seq[GenericTerm](StructTerm("paula",Seq[GenericTerm]()),StructTerm("p5",Seq[GenericTerm]()),DoubleTerm(0.4)))
           ,
            StructTerm("principle",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("p1",Seq[GenericTerm]()),DoubleTerm(0.8)))
           ,
            StructTerm("principle",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("p2",Seq[GenericTerm]()),DoubleTerm(0.2)))
           ,
            StructTerm("principle",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("p3",Seq[GenericTerm]()),DoubleTerm(0.6)))
           ,
            StructTerm("principle",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("p4",Seq[GenericTerm]()),DoubleTerm(0.4)))
           ,
            StructTerm("principle",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("p5",Seq[GenericTerm]()),DoubleTerm(0.2)))
           ,
            StructTerm("intention",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("p1",Seq[GenericTerm]()),DoubleTerm(0.6)))
           ,
            StructTerm("intention",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("p2",Seq[GenericTerm]()),DoubleTerm(0.1)))
           ,
            StructTerm("intention",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("p3",Seq[GenericTerm]()),DoubleTerm(0.4)))
           ,
            StructTerm("intention",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("p4",Seq[GenericTerm]()),DoubleTerm(0.2)))
           ,
            StructTerm("intention",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("p5",Seq[GenericTerm]()),DoubleTerm(0.2)))
           ,
            StructTerm("weight",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("p1",Seq[GenericTerm]()),DoubleTerm(0.7)))
           ,
            StructTerm("weight",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("p2",Seq[GenericTerm]()),DoubleTerm(0.8)))
           ,
            StructTerm("weight",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("p3",Seq[GenericTerm]()),DoubleTerm(0.4)))
           ,
            StructTerm("weight",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("p4",Seq[GenericTerm]()),DoubleTerm(0.4)))
           ,
            StructTerm("weight",Seq[GenericTerm](StructTerm("tom",Seq[GenericTerm]()),StructTerm("p5",Seq[GenericTerm]()),DoubleTerm(0.4)))
           ,
            StructTerm("sum",Seq[GenericTerm](IntTerm(0)))
           ,
            StructTerm("weightSum",Seq[GenericTerm](IntTerm(0)))
           ,
            StructTerm("threshold",Seq[GenericTerm](DoubleTerm(0.8)))


         )

         def planApplicabilities()(implicit executionContext: ExecutionContext) = List[StructTerm](

                 )



      def apply(name: String, yellowPages: IYellowPages, MAS: ActorRef[IMessage], parent: IMessageSource): Behavior[IMessage] = {
           Behaviors.setup { context =>
             val yp: IYellowPages = yellowPages
             val bb: IBeliefBase[GenericTerm] = beliefBaseFactory()
             implicit val executionContext: ExecutionContext = ExecutionContext(
                            name = name,
                            agentType = agent_type,
                            agent = context,
                            yellowPages = yp,
                            beliefBase = bb,
                            logger = context.log,
                            goalParser = GoalParser,
                            parent = parent
                          )
             bb.assert(initBeliefs)
             bb.assert(planApplicabilities)

         val initiator = context.spawn(Intention(executionContext), "initiator")

         MAS ! ReadyMessage(context.self)
         Behaviors.receive {
           (context, message) =>
             message match {
               case StartMessage() =>


                 implicit val timeout: Timeout = 99999.seconds
                 implicit val ec = context.executionContext
                 implicit val scheduler = context.system.scheduler


                 //              initGoals.foreach( tuple => initiator ! SubGoalMessage(tuple._1,tuple._2,context.self))
                 initGoals.foreach(struct => {


                   val result: Future[IMessage] = initiator.ask[IMessage](ref => {
                     val subGoal = GoalParser.create_goal_message(struct, AkkaMessageSource(ref))
                     if (subGoal.isDefined)
                       subGoal.get
                     else
                       throw new RuntimeException(s"No plan for initial goal $struct")
                     }
                   )


                   //result.onComplete {
                   //  case Success(IntentionDoneMessage(r)) => IntentionDoneMessage(r)
                   //  case Failure(_) => IntentionErrorMessage(src = null)
                   //}

                   //Await.result(result, timeout.duration)

                   val res = Await.result(result, timeout.duration)

                   if(!res.isInstanceOf[IntentionDoneMessage]) {
                     throw new RuntimeException(s"Issue with initial goal $struct")
                     context.system.terminate()
                   }

                   //                context.ask[ISubGoalMessage, IMessage](initiator, ref => SubGoalMessage(tuple._1, tuple._2,name, ref)) {
                   //                  case Success(IntentionDoneMessage(_, _)) => IntentionDoneMessage()
                   //                  case Failure(_) => IntentionErrorMessage()
                   //                }
                 }
                 )

                 initiator ! InitEndMessage(context.self)
                 normal_behavior(MAS)

               //            case InitEndMessage(_) =>
               //              context.log.debug(f"$name: I have started, switching behavior")
               //              normal_behavior()
             }

         }
       }
     }

     def normal_behavior(MAS: ActorRef[IMessage])(implicit executionContext: ExecutionContext): Behavior[IMessage] = {
       Behaviors.setup { context =>

         val pool = Routers.pool(poolSize = 8)(
           Behaviors.supervise(Intention(executionContext)).onFailure[Exception](SupervisorStrategy.restart))

         val router = context.spawn(pool, "intention-pool")
         //MAS ! ReadyMessage(context.self)
         Behaviors.receive {
           (context, message) =>
             message match {
               case IntentionDoneMessage(s) =>
                 context.log.debug(f"${executionContext.name}: an intention was done by $s")
               case IntentionErrorMessage(c,s) =>
                 context.log.debug(f"${executionContext.name}: an intention was done by $s: $c")
               case SubGoalMessage(_, _, _) =>
                 router ! message.asInstanceOf[SubGoalMessage]
               case GoalMessage(m, ref) =>
                 m match {
                   case t: StructTerm =>
                     val subGoal = GoalParser.create_goal_message(t, ref)

                     if (subGoal.isDefined)
                       context.self ! subGoal.get
                     else
                       ref.asInstanceOf[AkkaMessageSource].src ! IntentionErrorMessage(NoPlanMessage(),AkkaMessageSource(executionContext.agent.self))


                 }

                case AskMessage(m, ref) =>
                                m match {
                                  case t: StructTerm =>
                                    val subGoal = GoalParser.create_test_message(t, ref)

                                    if (subGoal.isDefined)
                                      context.self ! subGoal.get
                                    else
                                      ref.asInstanceOf[AkkaMessageSource].src ! IntentionErrorMessage(NoPlanMessage(),AkkaMessageSource(executionContext.agent.self))
                                }
             case BeliefMessage(m, ref) =>
                  m match {
                    case t: StructTerm =>
                    if(executionContext.beliefBase.assertOne(t)) {
                      val subGoal = GoalParser.create_belief_message(t, ref)

                      if (subGoal.isDefined)
                        context.self ! subGoal.get
                    }
                  }

              case UnBeliefMessage(m, ref) =>
                   m match {
                     case t: StructTerm =>
                     if(executionContext.beliefBase.retractOne(t)) {
                       val subGoal = GoalParser.create_unbelief_message(t, ref)

                       if (subGoal.isDefined)
                         context.self ! subGoal.get
                     }
                   }
             }
             Behaviors.same
         }
       }
     }
   }



   object GoalParser extends IAgentGoalParser {
        override def create_goal_message(t: StructTerm, ref: IMessageSource) (implicit executionContext: ExecutionContext): Option[SubGoalMessage] = {
                                   if(t.matchOnlyFunctorAndArity("getnames",0)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_getnames_0, args, ref))
                                   } else  
                                   if(t.matchOnlyFunctorAndArity("init",1)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_init_1, args, ref))
                                   } else  
                                   if(t.matchOnlyFunctorAndArity("distanceSum",2)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_distanceSum_2, args, ref))
                                   } else  
                                   if(t.matchOnlyFunctorAndArity("distanceWeight",2)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_distanceWeight_2, args, ref))
                                   } else  
                                   if(t.matchOnlyFunctorAndArity("normalizedDistance",1)) {
                                     val args: Parameters = Parameters(t.terms.toList)
                                     Option(SubGoalMessage(adopt_achievement_normalizedDistance_1, args, ref))
                                   } else   {
                    Option.empty[SubGoalMessage]
                    }

                }

        override def create_belief_message(t: StructTerm, ref: IMessageSource) (implicit executionContext: ExecutionContext): Option[SubGoalMessage] = {
                        {
                    Option.empty[SubGoalMessage]
                    }

                }

         override def create_test_message(t: StructTerm, ref: IMessageSource) (implicit executionContext: ExecutionContext): Option[SubGoalMessage] = {
                                {
                            Option.empty[SubGoalMessage]
                            }
                        }
          override def create_unbelief_message(t: StructTerm, ref: IMessageSource) (implicit executionContext: ExecutionContext): Option[SubGoalMessage] = {
                                         {
                                     Option.empty[SubGoalMessage]
                                     }
                                 }



        }


      object adopt_achievement_getnames_0 extends IGoal {

        def execute(params: Parameters) (implicit executionContext: ExecutionContext) : Unit = {
         var vars = VarMap()

         vars("Self").bind_to(StringTerm(executionContext.name))
         vars("Source").bind_to(StringTerm(executionContext.src.name))
         vars("Parent").bind_to(StringTerm(executionContext.parent.name))






                 //plan 0 start

                         vars.clear()
                         vars("Self").bind_to(StringTerm(executionContext.name))
                         vars("Source").bind_to(StringTerm(executionContext.src.name))
                         vars("Parent").bind_to(StringTerm(executionContext.parent.name))
                             plan0(vars)
                             return
                          // plan 0 end


             executionContext.src.asInstanceOf[AkkaMessageSource].address() ! IntentionErrorMessage(NoApplicablePlanMessage(),AkkaMessageSource(executionContext.agent.self))

        }


                      def plan0(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                               val ex_L98194 = executionContext.beliefBase.bufferedQuery( StructTerm("agent",Seq[GenericTerm](vars("L98194"))) )
                                               while (ex_L98194.hasNext) {
                                                   val sol_L98194 = ex_L98194.next
                                                   if(sol_L98194.result) {
                                                   vars += ("Name" -> sol_L98194.bindings("L98194").asInstanceOf[GenericTerm])
                                                                       adopt_achievement_init_1.execute(Parameters(List( vars("Name")  )))

                                                   }
                                               }
                                           vars -= ("Name")


                     }


      }

      object adopt_achievement_init_1 extends IGoal {

        def execute(params: Parameters) (implicit executionContext: ExecutionContext) : Unit = {
         var vars = VarMap()

         vars("Self").bind_to(StringTerm(executionContext.name))
         vars("Source").bind_to(StringTerm(executionContext.src.name))
         vars("Parent").bind_to(StringTerm(executionContext.parent.name))






                 //plan 0 start

                         vars.clear()
                         vars("Self").bind_to(StringTerm(executionContext.name))
                         vars("Source").bind_to(StringTerm(executionContext.src.name))
                         vars("Parent").bind_to(StringTerm(executionContext.parent.name))
                         vars +=(   "Agent" -> params.l_params(0))

                             plan0(vars)
                             return
                          // plan 0 end


             executionContext.src.asInstanceOf[AkkaMessageSource].address() ! IntentionErrorMessage(NoApplicablePlanMessage(),AkkaMessageSource(executionContext.agent.self))

        }


                      def plan0(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                               val ex_L32906 = executionContext.beliefBase.bufferedQuery( StructTerm("principle",Seq[GenericTerm](vars("Agent"),vars("L32906"),vars("P"))) )
                                               while (ex_L32906.hasNext) {
                                                   val sol_L32906 = ex_L32906.next
                                                   if(sol_L32906.result) {
                                                   vars += ("X" -> sol_L32906.bindings("L32906").asInstanceOf[GenericTerm])
                                                                       adopt_achievement_distanceSum_2.execute(Parameters(List( vars("Agent") , vars("X")  )))

                                                   }
                                               }
                                           vars -= ("X")
                                               val ex_L72641 = executionContext.beliefBase.bufferedQuery( StructTerm("weight",Seq[GenericTerm](vars("Agent"),vars("X"),vars("L72641"))) )
                                               while (ex_L72641.hasNext) {
                                                   val sol_L72641 = ex_L72641.next
                                                   if(sol_L72641.result) {
                                                   vars += ("W" -> sol_L72641.bindings("L72641").asInstanceOf[GenericTerm])
                                                                       adopt_achievement_distanceWeight_2.execute(Parameters(List( vars("Agent") , vars("W")  )))

                                                   }
                                               }
                                           vars -= ("W")
                                          adopt_achievement_normalizedDistance_1.execute(Parameters(List( vars("Agent")  )))


                     }


      }

      object adopt_achievement_distanceSum_2 extends IGoal {

        def execute(params: Parameters) (implicit executionContext: ExecutionContext) : Unit = {
         var vars = VarMap()

         vars("Self").bind_to(StringTerm(executionContext.name))
         vars("Source").bind_to(StringTerm(executionContext.src.name))
         vars("Parent").bind_to(StringTerm(executionContext.parent.name))






                 //plan 0 start

                         vars.clear()
                         vars("Self").bind_to(StringTerm(executionContext.name))
                         vars("Source").bind_to(StringTerm(executionContext.src.name))
                         vars("Parent").bind_to(StringTerm(executionContext.parent.name))
                         vars +=(   "Agent" -> params.l_params(0))
                          vars +=(   "X" -> params.l_params(1))

                         val r0 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm("principle",Seq[GenericTerm](vars("Agent"),vars("X"),vars("P"))),StructTerm("intention",Seq[GenericTerm](vars("Agent"),vars("X"),vars("I"))))),StructTerm("weight",Seq[GenericTerm](vars("Agent"),vars("X"),vars("W"))))),StructTerm("sum",Seq[GenericTerm](vars("CurrentSum"))))),StructTerm("is",Seq[GenericTerm](vars("D"),StructTerm("*",Seq[GenericTerm](vars("W"),StructTerm("**",Seq[GenericTerm](StructTerm("-",Seq[GenericTerm](vars("P"),vars("I"))),IntTerm(2))))))))),StructTerm("is",Seq[GenericTerm](vars("NewSum"),StructTerm("+",Seq[GenericTerm](vars("CurrentSum"),vars("D"))))))))

                         if (r0.result) {
                             r0.bindings foreach { case (k, v) =>
                            // vars += (k -> v.asInstanceOf[GenericTerm])
                                      vars(k).bind_to(v)
                             }
                             plan0(vars)
                             return
                          }

                          // plan 0 end


             executionContext.src.asInstanceOf[AkkaMessageSource].address() ! IntentionErrorMessage(NoApplicablePlanMessage(),AkkaMessageSource(executionContext.agent.self))

        }


                      def plan0(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("-", StructTerm("sum",Seq[GenericTerm](vars("CurrentSum")))),GoalParser)
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("sum",Seq[GenericTerm](vars("NewSum")))),GoalParser)


                     }


      }

      object adopt_achievement_distanceWeight_2 extends IGoal {

        def execute(params: Parameters) (implicit executionContext: ExecutionContext) : Unit = {
         var vars = VarMap()

         vars("Self").bind_to(StringTerm(executionContext.name))
         vars("Source").bind_to(StringTerm(executionContext.src.name))
         vars("Parent").bind_to(StringTerm(executionContext.parent.name))






                 //plan 0 start

                         vars.clear()
                         vars("Self").bind_to(StringTerm(executionContext.name))
                         vars("Source").bind_to(StringTerm(executionContext.src.name))
                         vars("Parent").bind_to(StringTerm(executionContext.parent.name))
                         vars +=(   "Agent" -> params.l_params(0))
                          vars +=(   "W" -> params.l_params(1))

                         val r0 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm("weightSum",Seq[GenericTerm](vars("CurrentSumWeight"))),StructTerm("is",Seq[GenericTerm](vars("NewSumWeight"),StructTerm("+",Seq[GenericTerm](vars("CurrentSumWeight"),vars("W"))))))))

                         if (r0.result) {
                             r0.bindings foreach { case (k, v) =>
                            // vars += (k -> v.asInstanceOf[GenericTerm])
                                      vars(k).bind_to(v)
                             }
                             plan0(vars)
                             return
                          }

                          // plan 0 end


             executionContext.src.asInstanceOf[AkkaMessageSource].address() ! IntentionErrorMessage(NoApplicablePlanMessage(),AkkaMessageSource(executionContext.agent.self))

        }


                      def plan0(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("-", StructTerm("weightSum",Seq[GenericTerm](vars("CurrentSumWeight")))),GoalParser)
                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("weightSum",Seq[GenericTerm](vars("NewSumWeight")))),GoalParser)


                     }


      }

      object adopt_achievement_normalizedDistance_1 extends IGoal {

        def execute(params: Parameters) (implicit executionContext: ExecutionContext) : Unit = {
         var vars = VarMap()

         vars("Self").bind_to(StringTerm(executionContext.name))
         vars("Source").bind_to(StringTerm(executionContext.src.name))
         vars("Parent").bind_to(StringTerm(executionContext.parent.name))






                 //plan 0 start

                         vars.clear()
                         vars("Self").bind_to(StringTerm(executionContext.name))
                         vars("Source").bind_to(StringTerm(executionContext.src.name))
                         vars("Parent").bind_to(StringTerm(executionContext.parent.name))
                         vars +=(   "Agent" -> params.l_params(0))

                         val r0 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm("sum",Seq[GenericTerm](vars("Xtest"))),StructTerm("threshold",Seq[GenericTerm](vars("T"))))),StructTerm("weightSum",Seq[GenericTerm](vars("Ytest"))))))

                         if (r0.result) {
                             r0.bindings foreach { case (k, v) =>
                            // vars += (k -> v.asInstanceOf[GenericTerm])
                                      vars(k).bind_to(v)
                             }
                             plan0(vars)
                             return
                          }

                          // plan 0 end


             executionContext.src.asInstanceOf[AkkaMessageSource].address() ! IntentionErrorMessage(NoApplicablePlanMessage(),AkkaMessageSource(executionContext.agent.self))

        }


                      def plan0(vars: VarMap)(implicit executionContext: ExecutionContext): Unit = {

                                          if(( ( (IntTerm(1) -  (vars("Xtest") / vars("Ytest")) )  > vars("T")) ).holds) {
                                                                  PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("The perceived integrity of ") + vars("Agent"))  + StringTerm(" is: "))  +  (IntTerm(1) -  (vars("Xtest") / vars("Ytest")) ) )  + StringTerm(". And is thus integer")) )))

                                          }
                                           else {
                                                                  PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( ( ( ( (StringTerm("The perceived integrity of ") + vars("Agent"))  + StringTerm(" is: "))  +  (IntTerm(1) -  (vars("Xtest") / vars("Ytest")) ) )  + StringTerm(". And is thus not integer")) )))

                                           }


                     }


      }





 }
object test_companion { 
   def create() = new test().agentBuilder 
   def create(in_coms : AgentCommunicationLayer) = new test(coms = in_coms).agentBuilder 
   def create(in_beliefBaseFactory: IBeliefBaseFactory) = new test(beliefBaseFactory = in_beliefBaseFactory).agentBuilder 
   def create(in_coms : AgentCommunicationLayer, in_beliefBaseFactory: IBeliefBaseFactory) = new test(coms = in_coms, beliefBaseFactory = in_beliefBaseFactory).agentBuilder 
} 
