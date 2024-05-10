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

 class tomY  (coms: AgentCommunicationLayer = new  DefaultCommunications,
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

                   case tomY.this.adopt_test_benevolent_2 =>
                     tomY.this.adopt_test_benevolent_2.execute(message.params.asInstanceOf[Parameters])

                   case tomY.this.adopt_test_valueprofile_3 =>
                     tomY.this.adopt_test_valueprofile_3.execute(message.params.asInstanceOf[Parameters])


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

         override def agent_type: String = "tomY"

         var vars = VarMap()

         def initGoals()(implicit executionContext: ExecutionContext) = List[StructTerm](
         )

         def initBeliefs()(implicit executionContext: ExecutionContext) = List[StructTerm](
                     StructTerm("myvalueprofile",Seq[GenericTerm](IntTerm(3),IntTerm(9)))
           ,
            StructTerm("inplan",Seq[GenericTerm](StructTerm("plan2",Seq[GenericTerm]()),StructTerm("shop",Seq[GenericTerm]())))
           ,
            StructTerm("inplan",Seq[GenericTerm](StructTerm("plan2",Seq[GenericTerm]()),StructTerm("wine",Seq[GenericTerm]())))
           ,
            StructTerm("inplan",Seq[GenericTerm](StructTerm("plan1",Seq[GenericTerm]()),StructTerm("shop",Seq[GenericTerm]())))
           ,
            StructTerm("initial",Seq[GenericTerm](StructTerm("plan1",Seq[GenericTerm]())))
           ,
            StructTerm("phi",Seq[GenericTerm](StructTerm("comfort",Seq[GenericTerm]()),StructTerm("plan1",Seq[GenericTerm]()),DoubleTerm(0.6)))
           ,
            StructTerm("phi",Seq[GenericTerm](StructTerm("comfort",Seq[GenericTerm]()),StructTerm("plan2",Seq[GenericTerm]()),DoubleTerm(0.5)))
           ,
            StructTerm("phi",Seq[GenericTerm](StructTerm("fun",Seq[GenericTerm]()),StructTerm("plan1",Seq[GenericTerm]()),DoubleTerm(0.3)))
           ,
            StructTerm("phi",Seq[GenericTerm](StructTerm("fun",Seq[GenericTerm]()),StructTerm("plan2",Seq[GenericTerm]()),DoubleTerm(0.3)))
           ,
            StructTerm("benevolence",Seq[GenericTerm](StructTerm("comfort",Seq[GenericTerm]()),DoubleTerm(0.2)))
           ,
            StructTerm("benevolence",Seq[GenericTerm](StructTerm("fun",Seq[GenericTerm]()),DoubleTerm(0.2)))
           ,
            StructTerm(":-",Seq[GenericTerm](StructTerm("wrongplan",Seq[GenericTerm](vars("X"),vars("V"))),StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm(",",Seq[GenericTerm](StructTerm("phi",Seq[GenericTerm](vars("V"),vars("X"),vars("Y"))),StructTerm("benevolence",Seq[GenericTerm](vars("V"),vars("Z"))))),StructTerm("initial",Seq[GenericTerm](vars("Plan"))))),StructTerm("phi",Seq[GenericTerm](vars("V"),vars("Plan"),vars("T"))))),StructTerm("is",Seq[GenericTerm](vars("Ben"),StructTerm("+",Seq[GenericTerm](vars("Y"),vars("Z"))))))),StructTerm(">",Seq[GenericTerm](vars("T"),vars("Ben")))))))
           ,
            StructTerm(":-",Seq[GenericTerm](StructTerm("acceptable",Seq[GenericTerm](vars("X"))),StructTerm("not",Seq[GenericTerm](StructTerm("wrongplan",Seq[GenericTerm](vars("X"),vars("_")))))))


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
                     {
                    Option.empty[SubGoalMessage]
                    }

                }

        override def create_belief_message(t: StructTerm, ref: IMessageSource) (implicit executionContext: ExecutionContext): Option[SubGoalMessage] = {
                     {
                    Option.empty[SubGoalMessage]
                    }

                }

         override def create_test_message(t: StructTerm, ref: IMessageSource) (implicit executionContext: ExecutionContext): Option[SubGoalMessage] = {
                                                   if(t.matchOnlyFunctorAndArity("benevolent",2)) {
                                                     val args: Parameters = Parameters(t.terms.toList)
                                                     Option(SubGoalMessage(adopt_test_benevolent_2, args, ref))
                                                   } else  
                                                   if(t.matchOnlyFunctorAndArity("valueprofile",3)) {
                                                     val args: Parameters = Parameters(t.terms.toList)
                                                     Option(SubGoalMessage(adopt_test_valueprofile_3, args, ref))
                                                   } else   {
                            Option.empty[SubGoalMessage]
                            }
                        }
          override def create_unbelief_message(t: StructTerm, ref: IMessageSource) (implicit executionContext: ExecutionContext): Option[SubGoalMessage] = {
                                      {
                                     Option.empty[SubGoalMessage]
                                     }
                                 }



        }


      object adopt_test_benevolent_2 extends IGoal {

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
                         vars +=(   "Y" -> params.l_params(0))
                          vars +=(   "X" -> params.l_params(1))

                         val r0 = executionContext.beliefBase.query(StructTerm(",",Seq[GenericTerm](StructTerm("inplan",Seq[GenericTerm](vars("Z"),vars("X"))),StructTerm("acceptable",Seq[GenericTerm](vars("Z"))))))

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

                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println( (StringTerm("acceptable plan ") + vars("Z")) )))
                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => coms.inform(StructTerm("agentX",Seq[GenericTerm]()),StructTerm("benevolent",Seq[GenericTerm](StructTerm("agentY",Seq[GenericTerm]()),vars("X"))))))


                     }


      }

      object adopt_test_valueprofile_3 extends IGoal {

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
                         vars +=(   "X" -> params.l_params(0))
                          vars +=(   "Y" -> params.l_params(1))
                          vars +=(   "Z" -> params.l_params(2))

                         val r0 = executionContext.beliefBase.query(StructTerm("myvalueprofile",Seq[GenericTerm](vars("Y"),vars("Z"))))

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

                                           BeliefUpdateAction.execute(BeliefUpdateAction.Parameters("+", StructTerm("valueprofile",Seq[GenericTerm](vars("Self"),vars("Y"),vars("Z")))),GoalParser)
                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => println(StructTerm("valueprofile",Seq[GenericTerm](vars("Self"),vars("Y"),vars("Z"))))))
                                          PrimitiveAction.execute(PrimitiveAction.Parameters(() => coms.inform(StructTerm("agentX",Seq[GenericTerm]()),StructTerm("valueprofile",Seq[GenericTerm](vars("Self"),vars("Y"),vars("Z"))))))


                     }


      }





 }
object tomY_companion { 
   def create() = new tomY().agentBuilder 
   def create(in_coms : AgentCommunicationLayer) = new tomY(coms = in_coms).agentBuilder 
   def create(in_beliefBaseFactory: IBeliefBaseFactory) = new tomY(beliefBaseFactory = in_beliefBaseFactory).agentBuilder 
   def create(in_coms : AgentCommunicationLayer, in_beliefBaseFactory: IBeliefBaseFactory) = new tomY(coms = in_coms, beliefBaseFactory = in_beliefBaseFactory).agentBuilder 
} 
