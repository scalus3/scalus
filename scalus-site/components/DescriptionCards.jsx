import Link from 'next/link'
import styles from './DescriptionCards.module.css'

export function DescriptionCards({ children }) {
  return <div className={styles.grid}>{children}</div>
}

export function DescriptionCard({ title, href, description, icon }) {
  const isExternal = href.startsWith('http')

  const CardContent = () => (
    <>
      <h3 className={styles.title}>
        {icon && <span className={styles.icon}>{icon}</span>}
        {title}
      </h3>
      {description && <p className={styles.description}>{description}</p>}
      <div className={styles.arrow}>→</div>
    </>
  )

  if (isExternal) {
    return (
      <a
        href={href}
        className={styles.card}
        target="_blank"
        rel="noopener noreferrer"
      >
        <CardContent />
      </a>
    )
  }

  return (
    <Link href={href} className={styles.card}>
      <CardContent />
    </Link>
  )
}

DescriptionCards.Card = DescriptionCard
